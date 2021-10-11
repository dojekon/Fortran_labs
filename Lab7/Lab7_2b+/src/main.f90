program lab_7_2b
use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, i, size, negatives = 0
   real(R_), allocatable   :: A(:)
   logical, allocatable    :: Mask(:)       

   open (file=input_file, newunit=In)
      read (In, *) size
      allocate(A(size))
      read (In, *) A
   close (In)

   Mask = A < 0

   negatives = count(mask)

   A=[Pack(A, .not. Mask), Pack(A, Mask)]

   do i = 1, size
      if ( A(i)>0) then
        call quicksort(A,i,size)
        exit
      end if
   end do

   open (file=output_file, newunit=Out)
        write(Out, fmt = '(10F10.2)') A(:)
    close (Out)

   contains
   
recursive subroutine quicksort(a, first, last)
  implicit none
  real(R_)  a(*), x, t
  integer first, last
  integer i, j

  x = a( (first+last) / 2 )
  i = first
  j = last
  do
     do while (a(i) < x)
        i=i+1
     end do
     do while (x < a(j))
        j=j-1
     end do
     if (i >= j) exit
     t = a(i);  a(i) = a(j);  a(j) = t
     i=i+1
     j=j-1
  end do
  if (first < i-1) call quicksort(a, first, i-1)
  if (j+1 < last)  call quicksort(a, j+1, last)
end subroutine quicksort

end program lab_7_2b
