program Shooting_project_Milad_Osman
    
    use grid
    use precondition
    use shooting
    use output
    use normalisation
    
    implicit none
    
    real(8), allocatable :: grid(:), y(:), y_left(:), y_right(:)
    real(8) h,e
    integer :: a,b,n,i

    a = 0
    b = 1
    n = 11

    call makegrid(a, b, n, h, grid)

    call threepoint(n, h, e, y)

    call trial(h, e, y, y_left, y_right)


    call normalise(a, b, h, y_left)
    call normalise(a, b, h, y_right)
 

end program Shooting_project_Milad_Osman
