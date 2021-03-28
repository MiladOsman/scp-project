program Shooting_project_Milad_Osman
    
    use input
    use grid
    use numerical_schroedinger
    use output
    
    implicit none
    
    integer :: a,b,n
    real(8) e, h ! eigenvalue and space between grid points
    real(8), allocatable :: y(:), mesh(:)

    a = 0 ! Start point
    b = 2 ! End point
    n = 100 ! Number of grid points

    call makegrid(a, b, n, h, mesh)

    call solve(a, b, n, e, y)

    ! call save(e, mesh, y)

end program Shooting_project_Milad_Osman
