program lab_7_9v
use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, i, size
   real(R_), allocatable   :: A(:,:), An

   open (file=input_file, newunit=In)
      read (In, *) size
      allocate(A(size,size))
      read (In, *) A
   close (In)
   
   open (file=output_file, newunit=Out)
      write(Out, fmt = '(a,F10.2)') "Результат вычислений:", maxval(sum(abs(A),dim=2))
   close (Out)

end program lab_7_9v
