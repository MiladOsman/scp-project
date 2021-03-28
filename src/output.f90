! Shooting project - Milad Osman
! Prints, saves and plots data

module output

    implicit none
    save
    private 
    public out, plotsave

    ! Overloaded interface for printing
    interface out
        module procedure integer_output
        module procedure real_output
        module procedure character_output
        module procedure one_d_array
        module procedure two_d_array
    end interface 
    
contains
    
    ! Prints integer
    subroutine integer_output(a)

        integer, intent(in) :: a 

        print *, a

    end subroutine integer_output

 !------------------------------------------------!
    
    ! Prints real
    subroutine real_output(a)

        real(8), intent(in) :: a 

        print *, a

    end subroutine real_output

 !------------------------------------------------!

    ! Prints characters
    subroutine character_output(a)

        character(:), allocatable :: a 

        print *, a

    end subroutine character_output

 !------------------------------------------------!

    ! Prints 1 dimenstional array
    subroutine one_d_array(a)

        real(8), intent(in) :: a(:)
        
        integer             :: i   

        do i=1, size(a)
            print *, a(i)
        end do

    end subroutine one_d_array

 !------------------------------------------------!

    ! Prints 2 dimenstional array
    subroutine two_d_array(a)

        real(8), intent(in) :: a(:,:)
        
        integer             :: i   

        do i=1, size(a, 1)
            print *, a(i,:)
        end do

    end subroutine two_d_array

 !------------------------------------------------!

    !
    subroutine save()

 

    end subroutine save
    
 !------------------------------------------------!  

    !
    subroutine plot()


    end subroutine plot

 !------------------------------------------------!

    ! 
    subroutine plotsave()


    end subroutine plotsave
    
 !------------------------------------------------!

end module output

