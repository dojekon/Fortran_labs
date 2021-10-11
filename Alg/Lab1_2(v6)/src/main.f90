program lab_1_2
    use Environment
    use ISO_Fortran_Env
   
    implicit none

    integer, parameter                :: ITEMS_AMOUNT = 5, TITLE_LEN = 20, COST_LEN =5, CURRENCY_LEN = 5
	
    character(kind=CH_)               :: Titles(ITEMS_AMOUNT, TITLE_LEN) = "", &
                                        Currencies(ITEMS_AMOUNT, CURRENCY_LEN) = ""

    integer                           :: Prices(ITEMS_AMOUNT) = 0

    character(:),       allocatable   ::  file_input, file_output

    file_input = "../data/input.txt"
    file_output = "output.txt"
         

   call readItemsList(file_input, Titles, Prices, Currencies) 
   call sortItems(Titles, Prices, Currencies)
   call outputItemsList(file_output, Titles, Prices, Currencies, "Результат", "append")

   contains
   ! Чтение списка товаров: Наименования, Цены, Валюта
   subroutine readItemsList(Input_File, Titles, Prices, Currencies)
        character(*)    Input_File
        character(kind=CH_) Titles(:, :), Currencies(:, :)
        integer         Prices(:)
        intent (in)     Input_File
        intent (out)    Titles, Currencies, Prices

        integer In, IO, i
        character(:), allocatable :: format

        open (file=Input_File, encoding=E_, newunit=In)
            format = '(' // TITLE_LEN //  'a1, i' // COST_LEN //  ', ' // CURRENCY_LEN // 'a1)'
            read (In, format, iostat=IO) (Titles(i, :), Prices(i), Currencies(i, :), i = 1, ITEMS_AMOUNT)
            call Handle_IO_status(IO, "Reading items list")
        close (In) 
    end subroutine readItemsList

    subroutine sortItems(Titles, Prices, Currencies)
        character (kind=CH_)     Titles(:,:), Currencies(:,:)
        integer                  Prices(:)
        intent (inout)           :: Titles, Currencies, Prices    

        integer                 tempPrices, i, Ind
        character (kind=CH_)    tempTitles(TITLE_LEN), tempCurrencies(CURRENCY_LEN)
        
        do i=1,3
             Ind = maxloc(Prices(i:Size(Prices)), dim = 1)
             Ind = Ind + i -1
            
             tempTitles = Titles(i,:)
             Titles(i,:) = Titles(Ind,:)
             Titles(Ind,:) = tempTitles

             tempPrices = Prices(i)
             Prices(i) = Prices(Ind)
             Prices(Ind)= tempPrices

             tempCurrencies = Currencies(i, :)
             Currencies(i, :) = Currencies(Ind, :)
             Currencies(Ind, :) = tempCurrencies
        enddo       
    end subroutine sortItems

    subroutine outputItemsList(Output_file, Titles, Prices, Currencies, ListName, Position)
        character(*)        Output_file, Position, ListName
        character(kind=CH_) Titles(:,:), Currencies(:,:)
        integer             Prices(:) 
        intent(in)         Output_file, Titles, Prices, Currencies, ListName, Position

        integer                     :: Out, i, IO
        character(:), allocatable   :: format

        open (file=Output_file, encoding=E_, position=Position, newunit=Out)
            write (out, '(/a)') ListName
            format = '(' // TITLE_LEN //  'a1, i' // COST_LEN //  ', ' // CURRENCY_LEN // 'a1)'
            write (Out, format, iostat=IO) &
                (Titles(i, :), Prices(i), Currencies(i, :), i =1, 3)
            call Handle_IO_status(IO, "writing " // ListName )            
        close (Out)
    end subroutine outputItemsList

    

end program lab_1_2
