! Shooting project - Milad Osman
! Normalises solutions 

module normalisation

    use integration
    use output

    implicit none
    save
    private 
    public normalise
    
contains

    subroutine normalise(h, y, ynorm)

        real(8), intent(in)               :: h        ! Distance between 2 grid points
        real(8), intent(in)               :: y(:)     ! Function to be normalised
        real(8), intent(out), allocatable :: ynorm(:) ! Normalised function
        
        real(8)                           :: integral ! Integral needed for normalisation
        real(8)                           :: norm     ! Normalisation factor

        allocate( ynorm( size(y) ))

        ynorm = abs(y**2)

        call newton_cotes(ynorm, h, 1, size(ynorm), integral)

        norm = sqrt( abs( 1._8 / integral ) )

        ynorm = ynorm * norm

    end subroutine normalise
    
end module normalisation