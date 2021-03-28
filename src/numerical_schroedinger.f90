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

    subroutine solve(a, b, n, e, y)

        !

        integer, intent(in)               :: a, b      ! The start and end point of the interval
        integer, intent(in)               :: n         ! Number of points the grid should have
        real(8), intent(out)              :: e         ! Final eigenvalue
        real(8), intent(out), allocatable :: y(:)      ! Final eigenvector
        
        real(8)                           :: h         ! Space between 2 grid points
        real(8), allocatable              :: mesh(:)   ! Discretised space
        real(8)                           :: e_prev    ! Eigenvalue from threepoint
        real(8), allocatable              :: y3(:)     ! Eigenvector from threepoint
        real(8), allocatable              :: yleft(:)  ! Inward eigenvector on subinterval
        real(8), allocatable              :: yright(:) ! Outward eigenvector on subinterval
        integer i
        real(8) e_next
        real(8)                           :: eps = 1d-15 !

        ! call out(eps)

        ! allocate( yleft(6), yright(6) )

        call makegrid(a, b, n, h, mesh)

        call threepoint(n, h, e_prev, y3)

        ! call out(e_prev)
        call shoot(h, e_prev, y3, yleft, yright)

        ! do i=1,size(yleft)
        !     print*,i,yleft(i),yright(i)
        ! enddo

        ! do i=1,1000
        !     call shoot(h, e_prev, y3, yleft, yright)
        
        !     call normalise(a, b, h, yleft)
        !     call normalise(a, b, h, yright)
        
        !     call correct(h, yleft, yright, e_prev, e_next)

        !     print*,'voor',e_prev,e_next,e_next-e_prev,i

        !     if ( abs( e_next - e_prev ) < eps) then 
        !         print*,'Exit---',e_prev,e_next,e_next-e_prev,i
        !         exit
        !     endif
        !     print*,'daar',e_prev,e_next,e_next-e_prev,i
        !     e_prev = e_next
        !     ! print*,'daarna',e_prev,e_next,e_next-e_prev,i
        ! end do

        ! call out(mesh)

    end subroutine solve

 !------------------------------------------------!

    subroutine shoot(h, e, y, yleft, yright)

        ! Makes trail eigenvectors between y(a) and y(x_m +1) and
        ! x = x_m - 1 and x = b

        real(8), intent(in)               :: y(:)      ! Eigenvector from threepoint
        real(8), intent(in)               :: h         ! Distance between 2 grid points
        real(8), intent(in)               :: e         ! Estimated eigenvalue by threepoint
        real(8), intent(out), allocatable :: yleft(:)  ! Outward trial solution
        real(8), intent(out), allocatable :: yright(:) ! Inward trial solution

        integer                           :: i         ! Loop index
        integer                           :: n         ! Size of eigenvector from threepoint   
        integer                           :: m         ! Matching point

        ! This way the number of grid points does not have to be passed separately 
        n = size(y)

        m = n / 2
    
        ! The +1 is needed for the derivative, which needs a value at x_m + 1
        allocate( yleft( m+1 ), yright( m+1 ) )
    
        yleft(1) = y(1); yleft(2) = y(2)
    
        yright(1) = y(n); yright(2) = y(n-1)

        ! The -2 is there because the first 3 terms are determined when i=0 and the size
        ! of the array is m+1
        do i=0, m-2
            yleft(3+i) = -yleft(1+i) -2._8* h**2 * e * yleft(2+i) + 2._8* yleft(2+i)
        enddo
       
        do i=0, m-2
            yright(3+i) = -yright(1+i)  -2._8* h**2 * e * yright(2+i) +  2._8* yright(2+i)
        end do

        do i=1, m+1
            print*,y(i),yleft(i),yright(i)
        enddo
        
        

        

        ! do i=0, m
        !     yright(3+i) = -yright(1+i)  -2._8* h**2 * e * yright(2+i) +  2._8* yright(2+i)
        ! end do
        ! print*,k
        
    
    end subroutine shoot

 !------------------------------------------------!

    subroutine correct(h, yleft, yright, e_prev, e_next)

        !

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

        ! call out(yright_int)
        ! call out(yleft_int)

        corr = .5_8 * ( yright_diff / yright(x_m) - yleft_diff / yleft(x_m) ) &
               * ( 1._8 / ( yleft_int / yleft(x_m)**2 + yright_int / yright(x_m)**2 ) )

        e_next = e_prev - corr

        ! print*,corr,e_prev,e_next
    end subroutine correct

end module numerical_schroedinger