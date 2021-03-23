! Shooting project - Milad Osman
!

module shooting

    use integration
    use grid

    implicit none
    save
    private 
    public yin, yout
    
contains

    real(8) function yin(h, e, y)

        real(8), intent(in) :: y(:)    ! Eigenvector
        real(8), intent(in) :: h       ! Distance between 2 grid points
        real(8), intent(in) :: e       ! Estimated energy/eigenvalue

        integer             :: i       ! Loop index

        yin = 0._8

        do i=1, 6
            yin = yin - y(i) + e * h**2 * y(6) + 2*y(6)
            print*,y(i),'LLLLLL',i
        end do
    
    end function yin
 !------------------------------------------------!

    real(8) function yout(h, e, y)

        real(8), intent(in) :: y(:)    ! Eigenvector
        real(8), intent(in) :: h       ! Distance between 2 grid points
        real(8), intent(in) :: e       ! Estimated energy/eigenvalue

        integer             :: i       ! Loop index

        yout = 0._8

        do i=11, 6, -1
            yout = yout - y(i) + e * h**2 * y(6) + 2*y(6)
            print*,y(i),'RRRRRR',i
        end do
    
    end function yout
 !------------------------------------------------!


    
end module shooting