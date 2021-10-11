program lab_one
use Environment

   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   character(:), allocatable  :: fmt
   integer                    :: In = 0, Out = 0
   real(R_)                   :: x, x2

   open (file=input_file, newunit=In)
      read (In, *) x
   close(In)

   x2 = x**(2)
   x = TAN(x/x2/(3-x2/(5-((x2)/(7)))));

   open (file=output_file, encoding=E_, newunit=Out)
      fmt = "(a, T7, '= ', f6.2)"
      write (Out, fmt) "x", x
   close (Out)

end program lab_one
