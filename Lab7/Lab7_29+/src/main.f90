program ex_7_29

use Environment

    implicit none
    integer, allocatable    :: A(:,:), Indexes(:), temp_col(:)
    integer                 :: xn = 0, yn = 0, In = 1, i = 0, pos = 0, Out = 0
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) xn, yn
        allocate(A(yn,xn))
        allocate(Indexes(yn))
        read(In, *) (A(i,:), i = 1, yn)
    close (In)
    
    do i=1,yn
        Indexes = maxloc(A(1:yn+1-i,1), dim = 1)
        pos = Indexes(1)

        temp_col = A(yn-i+1,1:xn)
        A(yn-i+1,1:xn) = A(pos,1:xn)
        A(pos,1:xn) = temp_col
    end do

    open (file=output_file, newunit=Out)
    write (Out, fmt = '('//xn//'i5)') (A(i,:), i = 1, yn)
    close (In) 
end program ex_7_29
