! Create a grid of a size that can be decided by the user

module grid

    implicit none
    save
    private
    public makegrid 
    
contains

    subroutine makegrid(a, b, n, mesh)

        integer, intent(in)               :: a,b     ! Boundries of the interval
        integer, intent(in)               :: n       ! Number of grid points
        real(8), intent(out), allocatable :: mesh(:) ! The grid itself

        real(8)                           :: h       ! Distance between 2 grid points
        integer                           :: i       ! Loop index

        h = ( dble(b) - dble(a) ) / ( dble(n) - 1 )

        allocate(mesh(n))

        mesh(1) = dble(a)
        
        mesh(2:) = (/ (h*i + a, i=1, n) /)  

    end subroutine makegrid

end module grid