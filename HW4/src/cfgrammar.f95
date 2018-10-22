module cfgrammar_module
    use parse_tools_module
    implicit none    

    type token_type
        LOGICAL :: is_terminal
        integer :: index
    end type

    type production_type
        type(token_type), dimension(:), allocatable :: tokens
    end type

    type productions_list_type
        type(production_type), dimension(:), allocatable :: pl
    end type

    type cfgrammar_type
        integer :: initial_nonterminal
        type(character_box_type), dimension(:), allocatable :: terminals
        type(character_box_type), dimension(:), allocatable :: nonterminals
        type(productions_list_type), dimension(:), allocatable :: productions

        contains
            procedure :: read_text_description, write_text_description, from_file, to_file
    end type     

contains
    subroutine read_text_description(self, unit)
        implicit none
        class(cfgrammar_type), intent(in out) :: self
        integer, intent(in) :: unit
    
        character(:), allocatable :: line, initial_nonterminal, rule_left_part, rule_right_part, production_str
        integer :: i, j, nonterminal_index, index, ier = 0
        type(character_box_type), dimension(:), allocatable :: tokens, productions, rule
        type(production_type) :: production
        type(token_type) :: token

        ! 1. Read initial nonterminal
        call read_line(UNIT, line, ier)
        initial_nonterminal = trim(line)

        ! 2. Read terminals
        call read_line(UNIT, line, ier)
        line = trim(line)
        self%terminals = split(line, " ")

        ! 3. Read nonterminals
        call read_line(UNIT, line, ier)
        line = trim(line)
        self%nonterminals = split(line, " ")
        self%initial_nonterminal = index_character_box(self%nonterminals, initial_nonterminal)

        ! 4. Read productions  
        allocate(self%productions(size(self%nonterminals)))
        do i = 1, size(self%productions)
            allocate(self%productions(i)%pl(0))
        end do
        
        call read_line(UNIT, line, ier)
        do while (.not. IS_IOSTAT_END(ier))  
            line = trim(line)          
            rule = split(line, "->")            
            rule_left_part = trim(rule(1)%s)
            rule_right_part = trim(rule(2)%s)
            nonterminal_index = index_character_box(self%nonterminals, rule_left_part)

            productions = split(rule_right_part, "|")
            do i = 1, size(productions)
                production_str = trim(productions(i)%s)
                tokens = split(production_str, " ")

                allocate(production%tokens(0))
                do j = 1, size(tokens)
                    if (tokens(j)%s == "_eps") cycle

                    index = index_character_box(self%terminals, tokens(j)%s)
                    if (index > 0) then 
                        token%is_terminal = .true.
                        token%index = index
                    else    
                        index = index_character_box(self%nonterminals, tokens(j)%s)
                        token%is_terminal = .false.
                        token%index = index
                    end if 

                    production%tokens = [production%tokens, token]
                end do
                
                self%productions(nonterminal_index)%pl = [self%productions(nonterminal_index)%pl, production]
                deallocate(production%tokens)
            end do            
            call read_line(UNIT, line, ier)
        end do
    end subroutine

    subroutine write_text_description(self, unit)
        implicit none
        class(cfgrammar_type), intent(in out) :: self
        integer, intent(in) :: unit
        
        type(token_type) :: token
        integer :: i, j, k
        character(:), allocatable :: str 
        ! 1. Write initial nonterminal
        write (unit, *) self%nonterminals(self%initial_nonterminal)%s

        ! 2. Write terminals
        do i = 1, size(self%terminals) - 1
            write (unit, '(a)', advance="no") self%terminals(i)%s // " "
        end do
        write (unit, '(a)') self%terminals(size(self%terminals))%s

        ! 3. Read nonterminals
        do i = 1, size(self%nonterminals) - 1
            write (unit, '(a)', advance="no") self%nonterminals(i)%s // " "
        end do
        write (unit, '(a)') self%nonterminals(size(self%nonterminals))%s

        ! 4. Read productions 
        do i = 1, size(self%productions)
            if (size(self%productions(i)%pl) == 0) cycle

            write (unit, '(a)', advance="no") self%nonterminals(i)%s // " -> "
            do j = 1, size(self%productions(i)%pl) 
                if (size(self%productions(i)%pl(j)%tokens) == 0) &
                    write (unit, '(a)', advance="no") "_eps"

                do k = 1, size(self%productions(i)%pl(j)%tokens)
                    token = self%productions(i)%pl(j)%tokens(k)
                    if (token%is_terminal) then
                        str = self%terminals(token%index)%s
                    else
                        str = self%nonterminals(token%index)%s
                    end if

                    if (k /= size(self%productions(i)%pl(j)%tokens)) str = str//" "
                    write (unit, '(a)', advance="no") str
                end do
                if (j /= size(self%productions(i)%pl)) then
                    write (unit, '(a)', advance="no") " | "
                else 
                    write (unit, *)
                end if
            end do
        end do
    end subroutine

    subroutine from_file(self, file_name)
        implicit none
        class(cfgrammar_type), intent(in out) ::self
        character(*), intent(in) :: file_name
        
        integer, parameter :: UNIT = 10
        open (UNIT, access = "sequential", file = file_name, status = 'old', action="read")
        call self%read_text_description(UNIT)
        close(UNIT)
    end subroutine

    subroutine to_file(self, file_name)
        implicit none
        class(cfgrammar_type), intent(in out) ::self
        character(*), intent(in) :: file_name

        integer, parameter :: UNIT = 10
        open (UNIT, access = "sequential", file = file_name, status = 'old', action="read")
        call self%write_text_description(UNIT)
        close(UNIT)
    end subroutine
end module cfgrammar_module