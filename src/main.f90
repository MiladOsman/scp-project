program Shooting_project_Milad_Osman
    
    use input
    use numerical_schroedinger
    use output
    
    implicit none
    
    integer :: a,b,n,i
    real(8) e
    real(8), allocatable :: y(:)

    a = 0
    b = 2
    n = 100

    call solve(a, b, n, e, y)
    

end program Shooting_project_Milad_Osman
