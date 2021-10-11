module Source_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Source_IO
   implicit none

contains
   pure recursive subroutine P2(unsorted)
   ! Ссылка на неотсортированный объект
      type(Surnames), pointer :: unsorted 

      if (Associated(unsorted)) then
      ! Выбрать минимум и вставить, начиная с unsorted
         call ChooseAndPaste(unsorted, unsorted, unsorted%next) 
         ! Продолжить сортировку неотсортированной части
         call P2(unsorted%next) 
      end if
   end subroutine P2

   pure recursive subroutine ChooseAndPaste(unsorted, minimum, current)
      type(Surnames), pointer :: unsorted ! переменная-ссылка (next) на первый неотсорт-ный объект
      type(Surnames), pointer :: minimum ! перем-ссылка (next) на миним объект из уже обработанных
      type(Surnames), pointer :: current ! текущ переменная-ссылка на объект-претендент быть минимумом
      type(Surnames), pointer :: tmp 

      if (Associated(current)) then
         if (current%surname < minimum%surname) then
            call ChooseAndPaste(unsorted, current, current%next)
         else ! текущая ссылка не оказалась лучше
            call ChooseAndPaste(unsorted, minimum, current%next)
         end if
      else ! Претенденты закончились
         if (.not. associated(unsorted, minimum)) then 
            tmp         => minimum
            minimum     => minimum%next
            tmp%next    => unsorted
            unsorted    => tmp
         end if
      end if
   end subroutine ChooseAndPaste
  
   pure recursive subroutine EraseSurnames(list)
      type(Surnames), pointer, intent(inout) :: list  

      type(Surnames), pointer                :: tmp  
      
      tmp => list%next
      deallocate(list)
      nullify(list)    

      if (associated(tmp)) & 
         call EraseSurnames(tmp)
   end subroutine EraseSurnames
end module Source_process
