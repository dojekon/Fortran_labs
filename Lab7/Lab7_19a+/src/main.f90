program ex_7_19a

use Environment

    implicit none
    integer, allocatable    :: B(:,:), Indexes(:, :), Ind_positive_pos(:,:)
    logical, allocatable    :: Mask(:)
    integer                 :: nX = 0, nY = 0, In = 1, Out = 0, i, j, N_positive_pos = 0
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) nX, nY
        allocate(B(nX,nY))
        allocate(Indexes(nX*nY,2))
        read (In, *) B
    close (In)

    allocate (Mask(nX*nY), source=.false.)

    Indexes(:, 1) = [((i, i = 1, nX), j = 1, nY)]
    Indexes(:, 2) = [((j, i = 1, nX), j = 1, nY)]

    Mask = [B > 0]
    N_positive_pos = Count(Mask)
    print *, Mask
    print *, "Кол-во положительных:", N_positive_pos

    allocate(Ind_positive_pos(N_positive_pos,2))

    Ind_positive_pos(:, 1) = Pack(Indexes(:,1), Mask)
    Ind_positive_pos(:, 2) = Pack(Indexes(:,2), Mask)




    open (file=output_file, newunit=Out)
       write(Out, *) N_positive_pos, 'положительных элемента найдены, координаты:'
      write(Out, *) 'X, Y'
     write(Out, '(I2,A1,I2)') (Ind_positive_pos(i,1), ',' ,Ind_positive_pos(i,2), i = 1, N_positive_pos)
    close (In)

end program ex_7_19a