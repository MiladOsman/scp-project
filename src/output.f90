! Shooting project - Milad Osman
! Prints, saves and plots data

module output

    implicit none
    save
    private 
    public out, save

    ! Overloaded interface for easier printing, for numbers only
    interface out
        module procedure integer_output
        module procedure real_output
        module procedure one_d_array
        module procedure two_d_array
    end interface 
    
contains
    
    subroutine integer_output(a)

        ! Prints integer

        integer, intent(in) :: a 

        print *, a

    end subroutine integer_output

 !------------------------------------------------!
    
    subroutine real_output(a)

        ! Prints real

        real(8), intent(in) :: a 

        print *, a

    end subroutine real_output

 !------------------------------------------------!

    subroutine one_d_array(a)

        ! Prints 1 dimenstional array as column

        real(8), intent(in) :: a(:)
        
        integer             :: i   

        do i=1, size(a)
            print *, a(i)
        end do

    end subroutine one_d_array

 !------------------------------------------------!

    subroutine two_d_array(a)

        ! Prints 2 dimenstional array

        real(8), intent(in) :: a(:,:)
        
        integer             :: i   

        do i=1, size(a, 1)
            print *, a(i,:)
        end do

    end subroutine two_d_array
    
 !------------------------------------------------!  

    subroutine save(e, x, y)

        real(8), intent(in) :: x(:) ! The x values of what has to be plotted
        real(8), intent(in) :: y(:) ! The y values of what has to be plotted
        real(8), intent(in) :: e ! Eigenvalue

        integer :: i                ! Loop index
        integer :: data             ! Name for data file to be opened
        integer :: plt              ! Name for plot file to be opened

        open(newunit=data, file='eigenstate.txt')

        ! Store the values in the opened file
        write(data,*) 'Eigenvalue:'
        write(data,*) e
        write(data,*) '-----------'

        write(data,*) 'x values, wavefunction:'
        do i=1, size(x)
            write(data,*) x(i), y(i)
        end do

        close(data)

    end subroutine save

end module output

