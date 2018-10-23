program cyk
    use cfgrammar_module
    use parse_tools_module
    implicit none
    
    type(cfgrammar_type), target :: grammar
    CHARACTER(1024) :: grammar_file_name, string_file_name
    character(:), allocatable :: name, extension, parsed_str, table_filename, tree_filename
    integer, dimension(:, :, :, :), allocatable :: table
    integer :: ext_index, unit, ier
    
    if (iargc() < 2) then
        print *, "No grammar file passed"
        print *, "USAGE: is_empty <grammar file> <parsed string file>"
        stop
    end if

    call getarg(1, grammar_file_name)
    call getarg(2, string_file_name)

    open (newunit=unit, access = "sequential", file = string_file_name, status = 'old', action="read")
    call read_line(unit, parsed_str, ier)
    parsed_str = trim(parsed_str)
    close(unit)

    call grammar%from_file(grammar_file_name)
    call grammar%normalize()
    table = grammar%build_cyk_table(parsed_str)
    
    ext_index = index(grammar_file_name, ".", .true.)
    name = grammar_file_name(:ext_index-1)
    extension = grammar_file_name(ext_index+1:)    

    ! write result to files
    call grammar%to_file(name//"_normalized."//extension)
    
    table_filename = name//"_table.csv"
    open (newunit=unit, access = "sequential", file = table_filename, status = 'replace', action="write")
    call print_cyk_table(unit, grammar, table)
    close(unit)

    tree_filename = name//"_tree.txt"
    open (newunit=unit, access = "sequential", file = tree_filename, status = 'replace', action="write")
    call print_parse_tree(unit, grammar, table, grammar%initial_nonterminal, 1, len(parsed_str), 0)
    close(unit)    
end program cyk