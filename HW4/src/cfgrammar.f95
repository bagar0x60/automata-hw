module cfgrammar_module
    implicit none

    type token_type
        LOGICAL :: is_terminal
        integer :: index
    end type 

    type string_type
        character(:), allocatable :: s
    end type

    type production_type
        type(token_type), dimension(:), allocatable :: tokens
    end type

    type productions_list_type
        type(production_type), dimension(:), allocatable :: pl
    end type

    type cfgrammar_type
        integer :: initial_nonterminal
        type(string_type), dimension(:), allocatable :: terminals
        type(string_type), dimension(:), allocatable :: nonterminals
        type(productions_list_type), dimension(:), allocatable :: productions
    end type     

contains
    function grammar_from_file(file_name)
        implicit none
    
        type(cfgrammar_type) :: grammar_from_file
        character(*) :: file_name
        type(string_type) :: s1, s2

        s1%s = "123"
        s2%s = "fuck trump"
        ! character(len=100) :: term
    
        !open (10, access = "sequential", file = file_name, status = 'old', action="read")
        !read (unit=10, fmt="(a1)", advance="no") term
        !print *, term
        !backspace(unit=10)
        !read (unit=10, fmt="(a2)", advance="no") term
        !print *, term
        !close(10)

        grammar_from_file%initial_nonterminal = 1
        

        ! allocate( grammar_from_file%terminals(0) )
        grammar_from_file%terminals = [s1, s2]
    end function
end module cfgrammar_module