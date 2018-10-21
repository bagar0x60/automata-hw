program main
    use cfgrammar_module
    implicit none
    
    type(cfgrammar_type) :: result
    result = grammar_from_file("../example.txt")
    print*, result%terminals(2)%s
end program main



