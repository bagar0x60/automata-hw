program main
    use cfgrammar_module
    use parse_tools_module
    implicit none
    
    type(cfgrammar_type) :: grammar
    call grammar%from_file("../2/test_3.txt")
    call grammar%normalize()
    call grammar%write_text_description(6)
end program main



