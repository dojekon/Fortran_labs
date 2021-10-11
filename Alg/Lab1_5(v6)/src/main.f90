program lab_1_5
  use Environment
    use ISO_Fortran_Env
    use IO
    implicit none

    character(*),       parameter   ::  file_input  = "../data/input.txt"
    character(*),       parameter   ::  file_output = "output.txt"

    type(item),      pointer     ::  items=>Null()

    items => readItemslist(file_input)
      if (Associated(items)) then
      call outputItemsList(file_output, items, "Исходный список:", "REWIND")

      call getRichB(items)
    endif
contains

    function findMaxPos(items, Ind)
      integer findMaxPos
      type(item),  pointer, intent(in)  ::  items
      type(item) :: it
      integer, intent(in) :: Ind(:)
      integer              :: max, pos
      integer  ::  curr_cost
      
      findMaxPos = 0
      pos = 1
      max = 0
      curr_cost = 0

      it = items
      
      do
        if((it%price > max) .AND. (.NOT. (ANY(Ind == pos)))) then 
         max = it%price
         findMaxPos = pos
        endif
        
        it = it%next
        pos = pos + 1
        if (.NOT. (Associated(it%next))) exit
      enddo          
    end function findMaxPos

    subroutine getRichB(items)
        type(item),  pointer, intent(in)  ::  items
        type(item) ::  it
        integer, allocatable :: Ind(:)
        INTEGER :: i, j, temp       
        integer :: out
        allocate(Ind(3))
        Ind = 0
        open (file = file_output, encoding = E_, newunit = Out)
        do i=1,3
          it = items

          temp = findMaxPos(items, Ind)
          Ind(i) = temp
          
          do j=1,Ind(i)
           if (j == Ind(i)) then       
                write (out, '(1a20, 1i5, 1a5)') it%title, it%price, it%currency       
           endif
           it = it%next
          enddo
        enddo  
        close (Out)
    end subroutine getRichB

    

end program lab_1_5
