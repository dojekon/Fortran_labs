program lab_8_6
use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, rows = 0, cols = 0, i
   integer, allocatable    :: C(:,:), A(:), B(:)
         

   open (file=input_file, newunit=In)
      read (In, *) rows, cols
      allocate(C(cols,rows))
      allocate(A(rows))
      allocate(B(cols))
      read(In, *) (C(:,i), i = 1, rows)
   close (In)
   
   call Func1(C,A,B)

   print *, "Rows sum"
   print *, A

   print *, "Columns sum"
   print *, B

   contains
   pure subroutine Func1(C, A, B)
      integer     C(:,:), A(:), B(:), ro, co
      intent(in)  C
      intent(out) A, B
      ! Формируем вектор сумм элементов строк
      do ro=1,rows
         do co=1,cols
            A(ro)=A(ro)+C(co,ro)
         enddo
      enddo

      ! Формируем вектор сумм элементов столбцов
      do co=1,cols
         do ro=1,rows
            B(co)=B(co)+C(co,ro)
         enddo
      enddo
   end subroutine Func1
end program lab_8_6
