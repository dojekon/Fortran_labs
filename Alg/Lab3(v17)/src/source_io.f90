module Source_IO
  use Environment         

   implicit none

   integer(I_), parameter :: SURNAME_LENGTH = 15

   type Surnames
      character(SURNAME_LENGTH, kind=1) :: surname
      type(Surnames), pointer           :: next => Null()
   end type Surnames       


contains

   function P1(inputFile) result (surnames_list)
      type(Surnames), pointer  :: surnames_list
      character(*), intent(in) :: inputFile

      integer(I_) :: in

      open (file=inputFile, encoding=E_, newunit=in)
         surnames_list => ReadSurname(in)     
      close (in)
   end function P1

   recursive function ReadSurname(in) result (surname)
      type(Surnames), pointer :: surname
      integer(I_), intent(in) :: in   

      integer(I_)                       :: IO
      character(SURNAME_LENGTH, kind=1) :: string

      read (in, '(a)', iostat=IO) string
      if (IO == 0) then
         allocate(surname)
         surname%surname = trim(string)  
         !trim возвращает строку с удалёнными конечными пробелами
         surname%next => ReadSurname(in) 
      else
         surname => Null()
      end if
   end function ReadSurname
!--------------------------------------------------
   subroutine P3(list, outputFile)
      type(Surnames), pointer, intent(in) :: list
      character(*), intent(in)            :: outputFile

      integer(I_) :: out

      open (file=outputFile, encoding=E_, newunit=out)
         call Output_one(Out, list) 
      close (out)
   end subroutine P3  
!--------------------------------------------------
   recursive subroutine Output_one(Out, Surname)
      integer, intent(in)        :: Out
      type(Surnames), intent(in)  :: Surname  
      
      integer  :: IO

      write (out, '(a)', iostat=IO) surname%surname
      call Handle_IO_status(IO, "writing surname")
      if (Associated(Surname%next)) &       
         call Output_one(Out, Surname%next)
   end subroutine Output_one   

end module Source_IO 
