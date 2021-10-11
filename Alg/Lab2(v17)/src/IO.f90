module IO
    use Environment

  implicit none

    ! Структура данных для хранения названий файлов
    type FileNames
        character(:, CH_), allocatable      :: String
        type(FileNames), pointer            :: Next => Null()
    end type FileNames

contains

    ! Чтение названий файлов
    function ReadFileNames(InputFile) result (Names)
        type(FileNames), pointer    :: Names
        character(*), intent(in)    :: InputFile
        integer                     :: In = 1
        
        open (file=InputFile, encoding=E_, newunit=In)
            Names => ReadFileName(in)
        close (In)
    end function readFileNames

    recursive function ReadFileName(in) result (Names)
        type(FileNames), pointer :: Names
        integer, intent(in)      :: In   
        integer, parameter       :: maxLen = 255
        character(maxLen, CH_)   :: string
        integer                  :: IO

        
        read (In, "(a)", iostat=IO) string
        call Handle_IO_Status(IO, "Reading filename")
        if (IO == 0) then
            allocate (Names)
            Names%String = string
            Names%Next => readFileName(In)
        else
            Names => Null()
        end if
    end function ReadFileName

    subroutine outputFileNames(OutputFile, Names)
        character(*), intent(in)    :: OutputFile
        type(FileNames), intent(in) :: Names
        integer :: Out

        open(file=OutputFile, encoding=E_, newunit=Out)
            call outputFileName(Out, Names)
        close(Out)
    end subroutine outputFileNames

    recursive subroutine outputFileName(Out, Names)
        integer, intent(in)          :: Out
        type(FileNames), intent(in) :: Names
        integer :: IO

        write (Out, "(a)", iostat=IO) Names%String
        call Handle_IO_Status(IO, "Writing filename to file")
        if (Associated(Names%Next)) &
            call outputFileName(Out, Names%Next)
    end subroutine outputFileName
        
end module IO
