program lab_six
use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, M(25), K = 11
   logical, allocatable    :: Pos(:)
   logical                 :: C



   contains
   ! Чистая подпрограмма в императивном стиле
   real(R_) pure subroutine ErfX(x)
      real(R_), intent(in) :: x 

      real(R_) R(4), Numerators(4), Denominators_Fuct(4), Denominators_Mult(4), x_8
      integer Ns(8), Plus

      ! Вычисление числителей
      Numerators = x ** [2,4,6,8]
      Numerators = Numerators * [-1, 1, -1, 1]
      ! =======================

      x_8 = Numerators(4) / x

      Denominators_Fuct = [1, 2 , 2*3 , 2*3*4]
      Ns = [5, 6, 7, 8, 9, 10, 11, 12]

      Denominators_Mult = [3, 5 , 7 , 9]
      Plus = 2

      R = Numerators / (Denominators_Fuct*Denominators_Mult)


      ! Сумма первых пяти членов
      ErfX = 1 + Sum(R)



   end subroutine ErfFunc

end program lab_six
