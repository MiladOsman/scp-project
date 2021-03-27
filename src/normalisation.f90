! Shooting project - Milad Osman
!

module normalisation

    use integration

    implicit none
    save
    private 
    public normalise
    
contains

    subroutine normalise(a, b, h, y)

        integer, intent(in)    :: a, b     ! Starting and ending point
        real(8), intent(in)    :: h        ! Distance between 2 grid points
        real(8), intent(inout) :: y(:)     ! Function to be normalised
        
        real(8)                :: integral ! Integral needed for normalisation
        real(8)                :: norm     ! Normalisation factor

        y = abs(y**2)

        call newton_cotes(y, h, a, b, integral)

        norm = sqrt( 1._8 / integral )

        y = y * norm

    end subroutine normalise
    
end module normalisation