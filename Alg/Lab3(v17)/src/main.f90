program lab_3
   use Environment
   use Source_Process
   use Source_IO

   implicit none           

   character(*), parameter :: input_file  = '../data/list.txt'
   character(*), parameter :: output_file = 'output.txt'

   type(Surnames), pointer :: list 
  
   list => P1(input_file)

   if (associated(list)) then
      if (associated(list%next)) then 
         call P2(list)         
      end if               
      call P3(list, output_file)
      call EraseSurnames(list)
   end if

   if (associated(list)) &
      print *, 'list', list%surname
end program lab_3
