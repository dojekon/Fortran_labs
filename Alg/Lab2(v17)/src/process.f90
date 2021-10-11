module Process
    use Environment
    use IO

    implicit none

contains

    !Фильтрация названий файлов
  recursive function FilterNames(unfilteredNames, extension) result (filteredNames)
        type(FileNames), pointer                    :: filteredNames
        type(FileNames), intent(in)                 :: unfilteredNames
        character(:, CH_), allocatable, intent(in)       :: extension
        character(255) :: tempString = ""
        character(4) :: tempExt = ""
       
        read (unfilteredNames%String, "(a)") tempString
        read (extension, "(a)") tempExt

        ! Если строка содержит расширение
       if ((tempString(Index(tempString,".")+1: len(tempString))) == tempExt) then
        ! И если существует следующй элемент
           if (Associated(unfilteredNames%Next)) then
           ! Ищем дальше      
                filteredNames => FilterNames(unfilteredNames%Next, extension)               
            end if
        !  Если строка не содержит расширения, занесём её в список 
        else
            allocate(filteredNames)
            filteredNames%String = unfilteredNames%String
              if (Associated(unfilteredNames%Next)) then
                 filteredNames%Next => FilterNames(unfilteredNames%Next, extension)   
              end if     
            end if           
    end function FilterNames       

   ! function 
end module Process
