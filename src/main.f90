program Shooting_project_Milad_Osman
    
    use grid
    use precondition
    use shooting
    use output
    
    implicit none
    
    real(8), allocatable :: grid(:), y(:)
    real(8) h,e
    integer :: a,b,n

    a = 0
    b = 1
    n = 11

    call makegrid(a, b, n, h, grid)

    call threepoint(n, h, e, y)

    call yleft(h, e, y)

    ! call out(y(3))
    ! call out(e)
    ! call out(grid(6))
    ! call out( size(y) )

    

end program Shooting_project_Milad_Osman
