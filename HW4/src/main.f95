program main
    use cfgrammar_module
    use parse_tools_module
    implicit none
    
    type(cfgrammar_type), target :: grammar
    integer, dimension(:, :, :, :), allocatable :: table
    character(:), allocatable :: str

    call grammar%from_file("../2/test_2.txt")
    call grammar%normalize()
    call grammar%write_text_description(6)
    str = "((())())"
    table = grammar%build_cyk_table(str)
    ! tree = parse_tree(grammar, table, grammar%initial_nonterminal, 1, len(str))
    call print_parse_tree(grammar, table, grammar%initial_nonterminal, 1, len(str), 0)
    ! call print_cyk_table(grammar, table)
end program main



