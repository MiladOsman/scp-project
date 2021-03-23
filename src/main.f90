program Shooting_project_Milad_Osman
    
    use grid
    use precondition
    use shooting
    use output
    
    implicit none
    
    real(8), allocatable :: grid(:), y(:)
    real(8) h,e,yleft,yright
    integer :: a,b,n

    a = 0
    b = 1
    n = 11


    call makegrid(a, b, n, h, grid)

    call threepoint(n, h, e, y)

    yleft = yin(h, e, y)
    yright = yout(h, e, y)

    print*,yleft,yright

    if (yleft==yright)print*,'hallo'
    

end program Shooting_project_Milad_Osman
