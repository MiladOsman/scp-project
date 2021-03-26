! Shooting project - Milad Osman
! Starts de three-point scheme for the preconditioning of the shooting method

module precondition

    use diagonalization
    use output

    implicit none
    save
    private 
    public threepoint
    
contains

    subroutine threepoint(n, h, e, y)

        integer, intent(in)  :: n            ! Size of grid
        real(8), intent(in)  :: h            ! Distance between 2 grid points
        real(8), intent(out), allocatable :: y(:)         ! Used for integration
        real(8), intent(out) :: e         ! Used for integration

        integer              :: i            ! Loop index
        real(8)              :: s(n,n)       ! S matrix
        real(8)              :: v(n,n)       ! V matrix
        real(8)              :: l(n,n)       ! L matrix
        real(8)              :: values(n)    ! Eigenvalues
        real(8)              :: vectors(n,n) ! Eigenvectors
        character(5)         :: numbers      ! Used for the format string

        allocate( y(n) )

        s = 0._8

        do i=1, n
            s(i,i) = -2._8
        end do
    
        do i=1, n-1
            s(i,i+1) = 1._8
            s(i+1,i) = 1._8
        end do

        v = 0._8

        l = -(1._8 / ( 2._8 * h**2) ) * s + v

        call diagonalize(l,vectors,values)

        ! Turn integer into character for format string
        ! write(numbers, '(i0)') n

        ! print *,'* Eigenvectors:'
        ! do i=1,n
        !     print '('//numbers//'f9.4)', vectors(i,:)
        ! end do

        y = vectors(:,1)

        ! call out(y)

        ! print *, '* Eigenvalues:'
        ! print '('//numbers//'f10.4)', values

        e = values(1)

    end subroutine threepoint
    
end module precondition