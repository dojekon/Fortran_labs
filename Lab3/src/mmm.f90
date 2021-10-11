

program lab_three
use Environment

implicit none
character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
!character(:), allocatable  :: fmt
integer                    :: In = 0, Out = 1
real(R_)                   :: x, y, z



!real, dimension(100) :: arr

 !  open (file=output_file, encoding=E_, newunit=Out)
  !    fmt = "(a, T7, '= ', f6.2)"
   !   write (Out, fmt) "z", z
   !close (Out)
call ArrMake(100)

contains

subroutine ArrMake(n)
  integer :: n, Out = 0, i
  character(*), parameter :: output_file = "../data/input.txt"
  !integer(4) :: seedvalue
  real(4) :: rnd

  open (file=output_file, encoding=E_, newunit=Out)
  do i = 0, n
    call random_seed()
    call random_number(rnd)
    write(*,*) rnd
    write (Out, *) rnd
  end do
  close (Out)
end subroutine ArrMake

real function Sum(x,y) result(r)
    real :: x,y ! Ввод
    r = x+y;   
end function Sum

end program lab_three