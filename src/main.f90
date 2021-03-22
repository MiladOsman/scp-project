program Shooting_project_Milad_Osman
    
    use grid
    use precondition
    use output
    
    implicit none
    
    real(8), allocatable :: grid(:)
    integer :: a,b,n,i
    real (8) :: hallo(5) = 1, doei (4,4) = 2, hoi = 3

    a = -3
    b = 3
    n = 7

    ! call makegrid(a,b,n,grid)

    ! do i=1,n
    !     print*,grid(i)
    ! enddo

    ! call threepoint(a,b,n)

    call out(doei)

end program Shooting_project_Milad_Osman
