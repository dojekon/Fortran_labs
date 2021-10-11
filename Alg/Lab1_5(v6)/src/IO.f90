module IO
    use Environment

    integer(I_),        parameter   ::  TITLE_LEN        = 20, &
                                        COST_LEN         = 5,  &
                                        CURRENCY_LEN     = 5

   type item

      character(TITLE_LEN, kind=CH_)        ::  title         = ""
      integer                               ::  price         = 0
      character(CURRENCY_LEN, kind=CH_)     ::  currency      = ""
      type(item), pointer                   ::  next     => Null()

   end type item

contains

    function readItemslist(file_input) result(items)
        type(item), pointer :: items
        character(*), intent(in)    :: file_input

        integer IN

        open(file = file_input, encoding=E_, newunit = IN)
            items => readItem(IN)
        close (IN)
    end function readItemslist

    recursive function readItem(In) result(itm)
        type(item), pointer         :: itm
        integer, intent(in)         :: In
        integer  IO
        character(:),   allocatable :: format

        allocate (itm)

        format = '(1a' // TITLE_LEN // ', 1i5, 1a' // CURRENCY_LEN //  ')'

        read (In, format, iostat=IO) itm%title, itm%price, itm%currency

        if (IO == 0) then
             itm%next => readItem(In)
         else
            deallocate(itm)
            nullify(itm)
         endif
        end function readItem

    ! Вывод списка
    subroutine outputItemsList(file_output, items, List_name, Position)
        character(*), intent(in)    :: file_output, Position, List_name
        type(item), intent(in)      :: items
        integer :: Out

        open (file = file_output, encoding = E_, position = Position, newunit = Out)
            write (out, '(/a)') List_name
            call outputItem(Out, items)
        close (Out)
    end subroutine outputItemsList

    ! Рекурсивный вывод элементов списка
    recursive subroutine outputItem(Out, itm)
        integer, intent(in)        :: Out
        type(item), intent(in)     :: itm

        integer  :: IO
        character(:), allocatable  :: format

        format = '(1a' // TITLE_LEN // ', 1i5, 1a' // CURRENCY_LEN //  ')'

        write (Out, format, iostat = IO) itm%title, &
                                          itm%price, &
                                          itm%currency
        call Handle_IO_status(IO, "Outputtin' item")
        if (Associated(itm%next)) &
            call outputItem(Out, itm%next)
    end subroutine outputItem

    

end module IO
