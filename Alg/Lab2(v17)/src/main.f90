program lab2
    use Environment
    use Process
    use IO

    !implicit none
    character(:), allocatable   :: F1, F2

    type(FileNames), pointer :: unfilteredFileNames => Null()
    type(FileNames), pointer :: filteredFileNames => Null()

    F1 = "../data/input.txt"
    F2 = "result.txt"

    unfilteredFileNames => ReadFileNames(F1)

    if(Associated(unfilteredFileNames)) then
       filteredFileNames => FilterNames(unfilteredFileNames, unfilteredFileNames%String)
        
      if(Associated(filteredFileNames)) &
        call outputFileNames(F2, filteredFileNames)
    end if


    
end program lab2
