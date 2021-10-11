program lab_four
use Environment

    implicit none
    character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
    integer                 :: In = 0, Out = 0, Nx = 0, Ny = 0, i = 0, j = 0
    real(R_)                :: x1 = 0, x2 = 0, dx = 0, y1 = 0, y2 = 0, dy = 0
    real(R_), allocatable   :: X(:), Y(:), F(:)


    open (file=input_file, newunit=In) ! Считываем входные данные
        read (In, *) x1, x2, dx
        read (In, *) y1, y2, dy
    close (In)

    open (file=output_file, newunit=Out) ! Вывод принятых переменных
      write (Out, '(3(a, T4, "= ", f0.4/))') "x1", x1, "x2", x2, "dx", dx
      write (Out, '(3(a, T4, "= ", f0.4/))') "y1", y1, "y2", y2, "dy", dy
    close (Out)   

    Nx = NInt((x2-x1) / dx) + 1 ! Определим количество вариантов значения переменной Х
    Ny = NInt((y2-y1) / dy) + 1 ! Определим количество вариантов значения переменной Y

    allocate (X(Nx*Ny), Y(Nx*Ny), F(Nx*Ny)) ! Выделяем память под массивы

    !call TabFuncImp(x1, y1, dx, dy, X, Y, F)
    call TabF(x1, y1, dx, dy, X, Y, F)

       open (file=output_file, encoding=E_, newunit=Out, position='append')
      write (Out, '("   x", T8, "|", T13, "y", T17, "|", T22, "f")')
      write (Out, '(f0.4, T8, "| ", f0.4, T17, "| ", f0.4)') ((X(i), Y(j), F(j + (i-1)*Ny), j = 1, Ny), i = 1, Nx)
   close (Out)


    contains

    pure subroutine TabFuncImp(x1, y1, dx, dy, X, Y, F)
      real(R_)    x1, y1, dx, dy, X(:), Y(:), F(:)
      intent(in)  x1, y1, dx, dy
      intent(out) X, Y, F
      integer     i, j

      do concurrent (i = 1:Nx)
         do concurrent (j = 1:Nx)  
            X(i) = x1 + dx*(i-1)
            Y(j) = y1 + dy*(j-1)
         end do
      end do

      do i = 1, Nx
         do j = 1, Ny
            F(j+(i-1)*Ny) = Sin(X(i)+Y(j)) / (((Cos(X(i))*Cos(Y(j)))**2)-1)
         end do
      end do
    end subroutine TabFuncImp  

    ! Чистая процедура в регулярном стиле
    ! Чистая процедура характеризуется тем, что не изменяет своих параметров
    ! Изменяет только те параметры, которые имеют тип связи INTENT(OUT)
   pure subroutine TabF(x1, y1, dx, dy, X, Y, F)
      real(R_)    x1, y1, dx, dy, X(:), Y(:), F(:)
      intent(in)  x1, y1, dx, dy
      intent(out) X, Y, F
      integer     i

      ! Формирование X = [x1, x2, x3, ...].
      X = [(x1 + dx*(i-1), i = 1, Nx)]
   
      ! Формирование Y = [y1, y2, y3, ...].
      Y = [(y1 + dy*(i-1), i = 1, Ny)]
   
      ! Формирование F = [F(x1,y1), F(x1,y2), F(x1,y3), ..., F(x2,y1), F(x2,y2), F(x2,y3), ...].
      ! F(1) == F(x1, y1), F(2) == F(x1, y2), .... F(Ny) = F(x1, yNy), ...
      F = [(((Sin(X(i)+Y(i))) / (((Cos(X(i))*Cos(Y(i)))**2)-1)), i = 1, Nx)]
   end subroutine TabF
end program lab_four
