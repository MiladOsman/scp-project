module precondition

    use diagonalization

    implicit none
    save
    private 
    public threepoint
    
contains

    subroutine threepoint(a,b,n)

        integer, intent(in) :: a,b          ! Boundries of the interval
        integer, intent(in) :: n            ! Number of grid points

        integer             :: i            ! Loop index
        real(8)             :: h            ! Distance between 2 grid points
        real(8)             :: s(n,n)       ! S matrix
        real(8)             :: v(n,n)       ! V matrix
        real(8)             :: l(n,n)       ! L matrix
        real(8)             :: values(n)    ! eigenvalues
        real(8)             :: vectors(n,n) ! eigenvectors
        character(5)        :: test         !
        
        h = ( dble(b) - dble(a) ) / dble(n)

        s = 0._8

        do i=1, n
            s(i,i) = -2._8
        end do
    
        do i=1, n-1
            s(i,i+1) = 1._8
            s(i+1,i) = 1._8
        end do

        v = 0._8

        l = -(1._8 / h**2) * s + v

        call diagonalize(l,vectors,values)

        print *,'* Eigenvectors:'
        do i=1,n
            print '(100f9.4)',vectors(i,:)
        end do

        print *,'* Eigenvalues:'
        print '(100f10.4)',values

    end subroutine threepoint
    
end module precondition