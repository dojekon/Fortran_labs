program lab_1_3
  use Environment
    use ISO_Fortran_Env
    use IO
    implicit none

    character(*),       parameter   ::  file_input  = "../data/input.txt"
    character(*),       parameter   ::  file_output = "../bin/output.txt"
    character(*),       parameter   ::  file_data   = "../bin/items.dat"

    type(item)                   ::  items(SIZE_)

    call Create_data_file(file_input, file_data)
    items = Read_items_list(file_data)
    call sortData(items, SIZE_)
    call Output_items_list(file_output, items, "Самые дорогие товары:", "append")
    contains

   subroutine sortData(items, SIZE_)
        type(item),  intent(inout)   ::  items(:)
        integer(I_),    intent(in)   ::  SIZE_
        integer(I_)                  ::  i, j
        integer,         allocatable ::  curr_cost(:), Ind(:)
        character(NAME_LEN, kind=CH_)        ::  temp_name         = ""  
        character(COST_LEN) :: temp_cost = ""

      do i=1,SIZE_
       allocate(curr_cost(SIZE_+1-i))

    !  Переменная cost имеет тип character, для приведения к integer 
    !  используется метод Read
       Read( items(i:SIZE_)%cost, "(i10)") curr_cost
       
       Ind = maxloc(curr_cost)
       
       temp_cost = items(i)%cost
       items(i)%cost = items(Ind(1)-1+i)%cost
       items(Ind(1)-1+i)%cost = temp_cost
         
       temp_name = items(i)%name
       items(i)%name = items(Ind(1)-1+i)%name
       items(Ind(1)-1+i)%name = temp_name

        deallocate(curr_cost)
      enddo
   end subroutine sortData
end program lab_1_3
