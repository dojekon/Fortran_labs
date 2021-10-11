module IO
    use Environment

    integer(I_),        parameter   ::  SIZE_ = 7
    integer(I_),        parameter   ::  NAME_LEN        = 20, &
                                        COST_LEN        = 5,  &
                                        CURR_LEN        = 5

   type item

      character(NAME_LEN, kind=CH_)        ::  name         = ""
      character(COST_LEN, kind=CH_)        ::  cost         = ""
      character(CURR_LEN, kind=CH_)        ::  currency     = ""

   end type item

contains
    ! Создание неформатированного файла данных
    subroutine Create_data_file(file_input, file_data)
        character(*), intent(in)    :: file_input, file_data
        type(item)                  :: it
        integer(I_)                 :: In, Out, IO, i, recl
        character(:), allocatable   :: format

        open (file = file_input, encoding = E_, newunit = In)
            recl = (NAME_LEN * CH_) + (COST_LEN * I_) + (CURR_LEN * CH_)
            open (file = file_data, form = 'unformatted', newunit = Out, access = 'direct', recl = recl)
                format = '(20a1, i5, 5a1)'
                do i = 1, SIZE_
                    read (In, format, iostat = IO) it%name, it%cost, it%currency
                    write (Out, iostat = IO, rec = i) it
                end do
            close (Out)
        close (In)
    end subroutine Create_data_file

    function Read_items_list(file_data) result(Items)
        type(item)               :: Items(SIZE_)
        character(*), intent(in)    :: file_data

        integer In, IO, recl

        recl = ((NAME_LEN * CH_) + (COST_LEN * I_) + (CURR_LEN * CH_))*SIZE_
        open (file = file_data, form = 'unformatted', newunit = In, access = 'direct', recl = recl)
            read (In, iostat = IO, rec = 1) Items
        close (In)
    end function Read_items_list

    ! Вывод списка класса.
    subroutine Output_items_list(file_output, items, List_name, Position)
        character(*), intent(in) :: file_output, Position, List_name
        type(item), intent(in) :: items(:)

        integer :: Out, IO, i
        character(:), allocatable :: format

        open (file = file_output, encoding = E_, position = Position, newunit = Out)
            write (out, '(/a)') List_name
            format = '(1a20, 1a5, 1a5)'
           do i=1,3
                write(Out, format, iostat = IO) items(i)%name, items(i)%cost, items(i)%currency
           enddo
        close (Out)
    end subroutine Output_items_list

    elemental subroutine str2int(str,int,stat)
        implicit none
        ! Arguments
        character(len=*),intent(in) :: str
        integer,intent(out)         :: int
        integer,intent(out)         :: stat

        read(str,*,iostat=stat)  int
  end subroutine str2int
end module IO
