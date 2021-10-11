real function Max(x, y) result(r)
    implicit none
    real :: x, y ! Ввод
    if (x>y) then
        r = x
    else 
        r = y 
    end if     
end function Max

real function Min(x, y) result(r)
    implicit none
    real :: x, y ! Ввод
    if (x<y) then
        r = x
    else 
        r = y 
    end if     
end function Min

program lab_two
use Environment

implicit none
character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
character(:), allocatable  :: fmt
integer                    :: In = 0, Out = 1
real(R_)                   :: x, y, z


open (file=input_file, newunit=In)
      read (In, *) x, y
close(In)

if (x<0) then
    z = Max(x,y)
else
    z = Min(x,y)
endif

   write(*,*) "z = ", z

   open (file=output_file, encoding=E_, newunit=Out)
      fmt = "(a, T7, '= ', f6.2)"
      write (Out, fmt) "z", z
   close (Out)


end program lab_two
