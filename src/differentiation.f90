! Shooting project - Milad Osman
!

module differentiation

    implicit none
    save
    private
    public derivative
    
contains

    !
    subroutine derivative(h, y, y_diff)

        real(8), intent(in)  :: h      ! Distance between 2 grid point
        real(8), intent(in)  :: y(:)   ! Eigenvector
        real(8), intent(out) :: y_diff ! The derivative of y

        y_diff = ( y(size(y)) - y(size(y)-2) ) / ( 2._8 * h )

    end subroutine derivative
    
end module differentiation