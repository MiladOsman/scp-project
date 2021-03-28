! Shooting project - Milad Osman
!

module numerical_schroedinger

    use grid
    use precondition
    use differentiation
    use integration
    use normalisation
    use output

    implicit none
    save
    private 
    public solve
    
contains

    !
    subroutine solve(a, b, n, e, y)

        integer, intent(in)               :: a, b      ! The start and end point of the interval
        integer, intent(in)               :: n        ! Number of points the grid should have
        real(8), intent(out)              :: e         ! Final eigenvalue
        real(8), intent(out), allocatable :: y(:)      ! Final eigenvector
        
        real(8)                           :: h         ! Space between 2 grid points
        real(8), allocatable              :: mesh(:)   ! Discretised space
        real(8)                           :: e3        ! Eigenvalue from threepoint
        real(8), allocatable              :: y3(:)     ! Eigenvector from threepoint
        real(8), allocatable              :: yleft(:)  ! Inward eigenvector on subinterval
        real(8), allocatable              :: yright(:) ! Outward eigenvector on subinterval
        integer i
        real(8) e_next

        call makegrid(a, b, n, h, mesh)

        call threepoint(n, h, e3, y3)
    
        call shoot(h, e3, y3, yleft, yright)

        ! do i=1,size(yleft)
        !     print*,i,yleft(i),yright(i)
        ! enddo
        
        call normalise(a, b, h, yleft)
        call normalise(a, b, h, yright)
        
        call correct(h, yleft, yright, e3, e_next)

    end subroutine solve

 !------------------------------------------------!

    ! Makes trail eigenvectors between x = a and x = x_m + 1 and
    ! x = x_m - 1 and x = b
    subroutine shoot(h, e, y, y_left, y_right)

        real(8), intent(in)               :: y(:)       ! Eigenvector from threepoint
        real(8), intent(in)               :: h          ! Distance between 2 grid points
        real(8), intent(in)               :: e          ! Estimated eigenvalue by threepoint
        real(8), intent(out), allocatable :: y_left(:)  ! Outward trial solution
        real(8), intent(out), allocatable :: y_right(:) ! Inward trial solution

        integer                           :: i          ! Loop index
        integer                           :: n          ! Size of eigenvector from threepoint   
        real(8)                           :: m          ! Middle point of interval

        ! This way the number of grid points does not have to be passed separately 
        n = size(y)

        m = ceiling( dble(n) / 2._8 ) 
        ! print*,m,int(m)+1,int(m/2)+1
        
        ! The +1 is needed for the derivative, which needs a value at x_m + 1
        allocate( y_left( int(m)+1 ), y_right( int(m)+1 ) )

        y_left(1) = y(1)  ; y_left(2) = y(2)
        y_right(1) = y(n) ; y_right(2) = y(n-1)
       
        do i=0, 4
            y_left(3+i) = -y_left(1+i)  -2._8* h**2 * e * y_left(2+i) +  2._8* y_left(2+i)
        end do

        do i=0, 4
            y_right(3+i) = -y_right(1+i)  -2._8* h**2 * e * y_right(2+i) +  2._8* y_right(2+i)
        end do
    
    end subroutine shoot

 !------------------------------------------------!

    !
    subroutine correct(h, yleft, yright, e_prev, e_next)

        real(8), intent(in)  :: yleft(:)    ! Outward trial solution
        real(8), intent(in)  :: yright(:)   ! Inward trial solution
        real(8), intent(in)  :: e_prev      ! Previous eigenvalue
        real(8), intent(in)  :: h           ! Distance between 2 grid points
        real(8), intent(out) :: e_next      ! New eigenvalue

        integer              :: x_m         ! Middle of the interval
        real(8)              :: corr        ! Correction difference
        real(8)              :: yleft_diff  ! Derivative at x_m from outward solution
        real(8)              :: yright_diff ! Derivative at x_m from inward solution
        real(8)              :: yleft_int   ! Integral of outward solution^2
        real(8)              :: yright_int  ! Integral of inward solution^2

        x_m = size(yleft) - 1

        call derivative(h, yleft, yleft_diff)
        call derivative(h, yright, yright_diff)

        call newton_cotes(yleft**2, h, 1, x_m, yleft_int)
        call newton_cotes(yright**2, h, 1, x_m, yright_int)

        call out(yright_diff)
        call out(yleft_diff)

        corr = .5_8 * ( yright_diff / yright(x_m) - yleft_diff / yleft(x_m) ) &
               * ( 1._8 / ( yleft_int / yleft(x_m)**2 + yright_int / yright(x_m)**2 ) )

        e_next = e_prev - corr

        ! print*,corr,e_prev,e_next
    end subroutine correct

end module numerical_schroedinger