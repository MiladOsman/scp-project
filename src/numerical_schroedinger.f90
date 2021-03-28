! Shooting project - Milad Osman
!

module numerical_schroedinger

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

    subroutine solve(a, b, n, e_next, y_norm)

        ! Main subroutine

        integer, intent(in)               :: a, b           ! The start and end point of the interval
        integer, intent(in)               :: n              ! Number of points the grid should have 
        real(8), intent(out)              :: e_next         ! Final eigenvalue
        real(8), intent(out), allocatable :: y_norm(:)           ! Final eigenvector
        
        real(8)                           :: h              ! Space between 2 grid points
        real(8)                           :: e_prev         ! Eigenvalue from threepoint
        real(8), allocatable              :: y3(:)          ! Eigenvector from threepoint
        real(8), allocatable              :: yleft(:)       ! Inward eigenvector on subinterval
        real(8), allocatable              :: yright(:)      ! Outward eigenvector on subinterval
        real(8), allocatable              :: yleft_norm(:)  ! Normalised outward eigenvector
        real(8), allocatable              :: yright_norm(:) ! Normalised inward eigenvector
        real(8), allocatable              :: y(:) ! Final eigenvector before normalisation
        integer                           :: i              ! Loop index
        integer                           :: protection     ! Limits number of iterations
        real(8)             :: eps = 1d-15     ! Tolerance

        call threepoint(n, h, e_prev, y3)
        
        protection = n * 30

        do i=1, protection
            call shoot(h, e_prev, y3, yleft, yright)
        
            call normalise(h, yleft, yleft_norm)
            call normalise(h, yright, yright_norm)

            
            call correct(h, yleft_norm, yright_norm, e_prev, e_next)

            if ( abs( e_next - e_prev ) < eps) exit

            e_prev = e_next

            if (i == protection) then 
                print *, 'Something went wrong:'
                print *, ' the eigenvalues should have converged by now'
            end if
        end do

        allocate(y( (size(yleft_norm)-1) *2 ) )

        do i=1, size(yleft_norm)-1
            y(i) = yleft_norm(i)
        end do

        do i=1, size(yright_norm)-1
            y( i + size(yright_norm)-1 ) = yright_norm( size(yright_norm) - i )
        end do

        call normalise(h, y, y_norm)

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
    
        yleft(1)  = y(1); yleft(2)  = y(2)
        yright(1) = y(n); yright(2) = y(n-1)

        ! The -2 is there because the first 3 terms are determined when i=0 and the size
        ! of the array is m+1
        do i=0, m-2
            yleft(3+i) = -yleft(1+i) -2._8* h**2 * e * yleft(2+i) + 2._8* yleft(2+i)
        enddo
       
        do i=0, m-2
            yright(3+i) = -yright(1+i)  -2._8* h**2 * e * yright(2+i) +  2._8* yright(2+i)
        end do
    
    end subroutine shoot

 !------------------------------------------------!

    subroutine correct(h, yleft, yright, e_prev, e_next)

        ! Corrects the eigenvalues

        real(8), intent(in)  :: yleft(:)    ! Outward trial solution
        real(8), intent(in)  :: yright(:)   ! Inward trial solution
        real(8), intent(in)  :: e_prev      ! Previous eigenvalue
        real(8), intent(in)  :: h           ! Distance between 2 grid points
        real(8), intent(out) :: e_next      ! New eigenvalue

        integer              :: m ,i        ! Middle of the interval
        real(8)              :: corr        ! Correction difference
        real(8)              :: yleft_diff  ! Derivative at x_m from outward solution
        real(8)              :: yright_diff ! Derivative at x_m from inward solution
        real(8)              :: yleft_int   ! Integral of outward solution^2
        real(8)              :: yright_int  ! Integral of inward solution^2

        m = size(yleft) - 1

        call derivative(h, yleft, yleft_diff)
        call derivative(h, yright, yright_diff)

        call newton_cotes(yleft(:m)**2, h, 1, size(yleft), yleft_int)
        call newton_cotes(yright(:m)**2, h, 1, size(yright), yright_int)
        
        corr = .5_8 * ( yright_diff / yright(m) - yleft_diff / yleft(m) ) &
               * ( 1._8 / ( yleft_int / yleft(m)**2 + yright_int / yright(m)**2 ) )

        e_next = e_prev - corr

    end subroutine correct

end module numerical_schroedinger