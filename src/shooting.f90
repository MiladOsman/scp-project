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

        real(8), intent(in) :: y(:)    ! Eigenvector/function from threepoint
        real(8), intent(in) :: h       ! Distance between 2 grid points
        real(8), intent(in) :: e       ! Estimated energy/eigenvalue by threepoint

        integer             :: i       ! Loop index
        integer :: n
        integer :: m
        real(8), allocatable :: y_left(:), y_right(:)

        n = size(y)
        m = int( ceiling( dble(n) / 2._8 ) )
        
        allocate( y_left(m), y_right(m) )

        y_left = 9.999999999999999_8; y_right = 9.999999999999999_8

        y_left(1) = y(1); y_left(2) = y(2)

        y_right(5) = y(n-1); y_right(6) = y(n)

        ! do i=1,6
        !     print*,y(i)
        ! enddo

        ! call out()

        ! dble(size(y))
        call out(0)
       
        ! do i=0, size(y_left)-1
        !     y_left(3+i) = -y_left(1+i)  -2* h**2 * e * y_left(2+i) +  2* y_left(2+i)
        ! end do

        ! do i=0, size(y_right)-1
        !     y_right(3+i) = -y_right(1+i)  -2* h**2 * e * y_right(2+i) +  2* y_right(2+i)
        ! end do

        ! call out(size(y))
        call out(y_left)
    
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