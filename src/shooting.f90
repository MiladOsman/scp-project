! Shooting project - Milad Osman
!

module shooting

    use integration
    use grid
    use output

    implicit none
    save
    private 
    public trial
    
contains

    !
    subroutine shoot()

    end subroutine shoot

 !------------------------------------------------!

    !
    subroutine trial(h, e, y, y_left, y_right)

        real(8), intent(in)               :: y(:)       ! Eigenvector from threepoint
        real(8), intent(in)               :: h          ! Distance between 2 grid points
        real(8), intent(in)               :: e          ! Estimated eigenvalue by threepoint
        real(8), intent(out), allocatable :: y_left(:)  ! Inward trial solution
        real(8), intent(out), allocatable :: y_right(:) ! Outward trial solution

        integer                           :: i          ! Loop index
        integer                           :: n          ! Size of eigenvector from threepoint   
        integer                           :: m          ! Middle point of interval

        n = size(y)
        m = int( ceiling( dble(n) / 2._8 ) ) + 1
        
        allocate( y_left(m), y_right(m) )

        y_left(1) = y(1); y_left(2) = y(2)

        y_right(1) = y(n); y_right(2) = y(n-1)
       
        do i=0, 4
            y_left(3+i) = -y_left(1+i)  -2._8* h**2 * e * y_left(2+i) +  2._8* y_left(2+i)
        end do

        do i=0, 4
            y_right(3+i) = -y_right(1+i)  -2._8* h**2 * e * y_right(2+i) +  2._8* y_right(2+i)
        end do
    
    end subroutine trial

 !------------------------------------------------!


    
end module shooting