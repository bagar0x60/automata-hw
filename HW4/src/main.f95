program main
    use cfgrammar_module
    implicit none
    
    type(cfgrammar_type) :: grammar
    call grammar%from_file("../example.txt")
    call grammar%write_text_description(6)
end program main



