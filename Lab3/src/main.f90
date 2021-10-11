program lab_three
use Environment

  implicit none
  character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
  integer                    :: In = 0, Out = 0, arr_size = 100, N = 0
  real(R_), allocatable      :: Arr(:)
  real :: Result = 0

  call RndMake(arr_size) ! Создадим файл со случайными числами
  
  open (file=input_file, newunit=In) ! Заполним массив числами из файла
    read (In,  *) N 
    allocate (Arr(N))

    read (In, *) Arr
  close (In)

  print *, SIZE(Arr(5:100:3))
  print *, " "
  print "(f6.3)", Arr(5:100:3)  
  print *, " "
  Result = Sum(Arr(5:100:3))

  open (file=output_file, encoding=E_, newunit=Out)
    write (Out, "(f6.3)") Result 
  close (Out)

contains

subroutine RndMake(n) ! Функция генерации случайных чисел
  integer :: n, Out = 0, i
  character(*), parameter :: output_file = "../data/input.txt"
  real(4) :: rnd

  open (file=output_file, encoding=E_, newunit=Out)
  write(Out, "(i10)") n
  do i = 1, n
    call random_seed()
    call random_number(rnd)
    write (Out, "(f6.3)") rnd 
  end do
close (Out)
end subroutine RndMake


end program lab_three
