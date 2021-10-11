program lab_7_39
use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 1, Out = 0, xn, yn, i = 0, j = 0, res
   integer, allocatable    :: A(:,:), B(:), Inpt(:,:)
         

   open (file=input_file, newunit=In)
         read(In, *) xn, yn
         allocate(Inpt(xn,yn))
         allocate(B(xn*yn))
      read (In, *) Inpt
   close (In)
      
   B = [((Inpt(i,j), i=1,10),j = 1,15)]
   B = pack(B, B<0)

   open (file=output_file, newunit=Out)
      write (Out, fmt = '(20(1x,i3))') B
   close (In) 

end program lab_7_39
