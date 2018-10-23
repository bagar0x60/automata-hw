program is_language_empty
    use cfgrammar_module
    use parse_tools_module
    implicit none
    
    type(cfgrammar_type), target :: grammar
    CHARACTER(1024) :: file_name
        
    if (iargc() == 0) then
        print *, "No grammar file passed"
        print *, "USAGE: is_empty <grammar file>"
        stop
    end if

    CALL getarg(1, file_name)
    call grammar%from_file(file_name)
    call grammar%normalize()
    if (grammar%is_empty()) then
        print *, "empty"
    else
        print *, "nope"
    end if
end program is_language_empty