program lab_1_4
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
    call sortData(items, 1, SIZE_)
    call Output_items_list(file_output, items, "Самые дорогие товары:", "append")
    contains

    recursive subroutine sortData(items, i, SIZE_)
        implicit none
        type(item),     intent(inout)   ::  items(:)
        integer(I_),    intent(in)      ::  SIZE_
        integer(I_),    intent(in)      ::  i
        character(NAME_LEN, kind=CH_)        ::  temp_name         = ""  
        character(COST_LEN)                  :: temp_cost = ""
        integer,         allocatable ::  curr_cost(:), Ind(:)

        allocate(curr_cost(SIZE_+1-i))

        Read( items(i:SIZE_)%cost, "(i10)") curr_cost
       
        Ind = maxloc(curr_cost)
       
        temp_cost = items(i)%cost
        items(i)%cost = items(Ind(1)-1+i)%cost
        items(Ind(1)-1+i)%cost = temp_cost
         
        temp_name = items(i)%name
        items(i)%name = items(Ind(1)-1+i)%name
        items(Ind(1)-1+i)%name = temp_name

        if(i < SIZE_) call sortData(items, i+1, SIZE_)
    end subroutine sortData
end program lab_1_4