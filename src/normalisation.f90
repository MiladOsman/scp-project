! ! Shooting project - Milad Osman
! !

! module normalisation

!     use integration

!     implicit none
!     save
!     private 
!     public normalise
    
! contains

!     subroutine normalise(a, b, h, y)

!         integer, inent
!         real(8), intent(in)    :: h ! Eigenfunction/vector
!         real(8), intent(inout) :: y(:) ! Eigenfunction/vector
!         real(8), intent(out)   :: norm ! Normalisation factor

!         y = abs(y**2)

!         call Newton_cotes(y,h)




!     end subroutine normalise
    
! end module normalisation