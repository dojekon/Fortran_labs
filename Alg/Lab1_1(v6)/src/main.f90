program lab_1_1
   use Environment
   use ISO_Fortran_Env

   integer, parameter              :: ITEMS_AMOUNT = 5, TITLE_LEN = 20, COST_LEN = 5, CURRENCY_LEN = 5
   character(:), allocatable         :: input_file, output_file, format

   ! Массивы названий, стоимостей, валют и временных переменных для обменов при сортировке
   character(TITLE_LEN, kind=CH_)         :: Titles(ITEMS_AMOUNT) = "", tmpTitle = ""

   integer                                :: Costs(ITEMS_AMOUNT) = 0, tmpCosts = 0

   character(COST_LEN, kind=CH_)          :: Currencies(ITEMS_AMOUNT) = "", tmpCurrency = ""

   integer                                :: In, Out, IO, i, j, Ind

   input_file = "../data/input.txt"
   output_file = "output.txt"

   
   open (file=input_file, encoding=E_, newunit=In)
      format = '(1a20, 1i5, 1a5)'
      read (In, format, iostat=IO) (Titles(i), Costs(i), Currencies(i), i = 1, ITEMS_AMOUNT)
   close (In)

      ! Обработка статуса чтения.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while reading items list."
      case(1:)
         write (Out, '(a)') "Error while reading items list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while reading items list: ", io
   end select

   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(a)') "Исходный список:"
      ! Пояснения к записи те же, что и к чтению.
      write (Out, format, iostat=IO) (Titles(i), Costs(i), Currencies(i), i = 1, ITEMS_AMOUNT)
   close (Out)
      Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing class list."
      case(1:)
         write (Out, '(a)') "Error while writing class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing class list: ", io
   end select
       
   do i=1,3
      Ind = maxloc(costs(i:ITEMS_AMOUNT), dim = 1)

      tmpCosts = Costs(i)
      Costs(i) = Costs(Ind-1+i)
      Costs(Ind-1+i) = tmpCosts
      
      tmpTitle= Titles(i)
      Titles(i) = Titles(Ind-1+i)
      Titles(Ind-1+i) = tmpTitle
   enddo

      format = '(1a20, 1i5, 1a5)'
   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (Out, '(/a)') "Самые дорогие товары:"
      write (Out, format, iostat=IO)  &
      (Titles(i), Costs(i), Currencies(i), i = 1, 3)
   close(Out)
      ! Обработка статуса записи.
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted boys list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted boys list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted boys list: ", io
   end select
end program lab_1_1