program normalizer
    use cfgrammar_module
    use parse_tools_module
    implicit none
    
    type(cfgrammar_type), target :: grammar
    CHARACTER(1024) :: file_name
    character(:), allocatable :: name, extension
    integer :: ext_index
    
    if (iargc() == 0) then
        print *, "No grammar file passed"
        print *, "USAGE: is_empty <grammar file>"
        stop
    end if

    CALL getarg(1, file_name)
    call grammar%from_file(file_name)
    call grammar%normalize()
    
    ext_index = index(file_name, ".", .true.)
    name = file_name(:ext_index-1)
    extension = file_name(ext_index+1:)
    call grammar%to_file(name//"_normalized."//extension)
end program normalizer