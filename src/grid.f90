! Shooting project - Milad Osman
! Creates a grid of a size that can be decided by the user

module grid

    use output

    implicit none
    save
    private
    public makegrid 
    
contains

    subroutine makegrid(a, b, n, h, mesh)

        integer, intent(in)               :: a,b     ! Boundries of the interval
        integer, intent(in)               :: n       ! Number of grid points
        real(8), intent(out), allocatable :: mesh(:) ! The grid itself
        real(8), intent(out)              :: h       ! Distance between 2 grid points
        
        integer                           :: i       ! Loop index

        h = ( dble(b) - dble(a) ) / ( dble(n)  )

        allocate(mesh(n))

        mesh(1) = dble(a)
        
        ! mesh = (/ ( h*i + dble(a), i=0, n-1 ) /) 

        do i=2, n+1
            mesh(i) = (i - 1) * h
        end do

        ! call out(h)
        ! print*,'\'

        ! mesh(n) = dble(b)

    end subroutine makegrid

end module grid