! Shooting project - Milad Osman
!

module shooting

    use integration
    use grid
    use output

    implicit none
    save
    private 
    public yleft, yright
    
contains

    subroutine yleft(h, e, y)

        real(8), intent(in) :: y(:)    ! Eigenvector/function
        real(8), intent(in) :: h       ! Distance between 2 grid points
        real(8), intent(in) :: e       ! Estimated energy/eigenvalue

        integer             :: i       ! Loop index

        i=3

        ! call out( y( size(y)-1 ) )
    
    end subroutine yleft
 !------------------------------------------------!

    real(8) function yright(h, e, y)

        real(8), intent(in) :: y(:)    ! Eigenvector
        real(8), intent(in) :: h       ! Distance between 2 grid points
        real(8), intent(in) :: e       ! Estimated energy/eigenvalue

        integer             :: i       ! Loop index

    
    
    end function yright
 !------------------------------------------------!


    
end module shooting