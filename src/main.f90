program Shooting_project_Milad_Osman
    
    use grid
    use precondition
    
    implicit none
    
    real(8), allocatable :: grid(:)
    integer :: a,b,n,i

    a = -3
    b = 3
    n = 7

    ! call makegrid(a,b,n,grid)

    ! do i=1,n
    !     print*,grid(i)
    ! enddo
    
    github

    call threepoint(a,b,n)

end program Shooting_project_Milad_Osman
