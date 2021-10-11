program lab_six
use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: erf_x = 0, x = 0
         

   open (file=input_file, newunit=In)
      read (In, *) x
   close (In)
   
  erf_x = ErfX(x)

      open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '(4(a, T16, "= ", e13.6/))') 'x', x, "Erf(x)", erf_x, "Fortran erf(x)", erf(x), "Error", erf_x - erf(x)
   close (Out)

   contains
   ! Чистая подпрограмма в императивном стиле
   real(R_) pure function ErfX(x)
      real(R_), intent(in) :: x 
      real(R_) R(4), Xp, Numerators(4), Denominators_Fuct(4), Denominators_Mult(4), x_8
      integer Ns(4)

      Xp = (2.0*x)/sqrt(4 * Atan(1._R_))

      ! Вычисление числителей
      Numerators = x ** [2,4,6,8]
      Numerators = Numerators * [-1, 1, -1, 1]
      ! =======================

      x_8 = Numerators(4) / x

      Denominators_Fuct = [1, 2 , 2*3 , 2*3*4]
      Ns = [1,2,3,4]

      Denominators_Mult = [3, 5, 7, 9]

      R = (Numerators / (Denominators_Fuct*Denominators_Mult)) * Xp

      ! Сумма первых пяти членов
      ErfX = Xp + Sum(R)

      do while ((ErfX + R(4)) /= ErfX)

         Numerators = Numerators * x_8

         Ns = Ns + 4

         Denominators_Fuct(1) = Denominators_Fuct(4) * Ns(1)
         Denominators_Fuct(2) = Denominators_Fuct(1) * Ns(2)
         Denominators_Fuct(3) = Denominators_Fuct(2) * Ns(3)
         Denominators_Fuct(4) = Denominators_Fuct(3) * Ns(4)
  
         Denominators_Mult = Denominators_Mult + 8

         R = (Numerators / (Denominators_Fuct*Denominators_Mult)) * Xp

         ErfX = ErfX + Sum(R)

         end do
   end function ErfX

end program lab_six
