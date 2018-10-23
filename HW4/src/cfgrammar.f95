module cfgrammar_module
    use parse_tools_module
    implicit none    

    type token_type
        LOGICAL :: is_terminal
        integer :: index
    end type

    type tokens_type
        type(token_type), dimension(:), allocatable :: tokens
    end type

    type production_type
        type(tokens_type), dimension(:), allocatable :: p
    end type

    type cfgrammar_type
        integer :: initial_nonterminal
        type(character_box_type), dimension(:), allocatable :: terminals
        type(character_box_type), dimension(:), allocatable :: nonterminals
        type(production_type), dimension(:), allocatable :: productions

        contains
            procedure :: read_text_description, write_text_description, from_file, &
                         to_file, normalize, is_empty, build_cyk_table
    end type   
contains
    subroutine read_text_description(self, unit)
        implicit none
        class(cfgrammar_type), intent(in out) :: self
        integer, intent(in) :: unit
    
        character(:), allocatable :: line, initial_nonterminal, rule_left_part, rule_right_part, production_str
        integer :: i, j, nonterminal_index, index, ier = 0
        type(character_box_type), dimension(:), allocatable :: tokens, productions, rule
        type(tokens_type) :: production
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
            allocate(self%productions(i)%p(0))
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
                
                self%productions(nonterminal_index)%p = [self%productions(nonterminal_index)%p, production]
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
            if (size(self%productions(i)%p) == 0) cycle

            write (unit, '(a)', advance="no") self%nonterminals(i)%s // " -> "
            do j = 1, size(self%productions(i)%p) 
                if (size(self%productions(i)%p(j)%tokens) == 0) &
                    write (unit, '(a)', advance="no") "_eps"

                do k = 1, size(self%productions(i)%p(j)%tokens)
                    token = self%productions(i)%p(j)%tokens(k)
                    if (token%is_terminal) then
                        str = self%terminals(token%index)%s
                    else
                        str = self%nonterminals(token%index)%s
                    end if

                    if (k /= size(self%productions(i)%p(j)%tokens)) str = str//" "
                    write (unit, '(a)', advance="no") str
                end do
                if (j /= size(self%productions(i)%p)) then
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

    subroutine create_nonterminal(self, index)
        implicit none
        class(cfgrammar_type), intent(in out) ::self
        character(:), allocatable :: label
        integer, intent(out) :: index
        type(character_box_type) :: character_box
        type(production_type) :: new_nonterminal_production

        integer :: new_label_index
        index = size(self%nonterminals) + 1
        new_label_index = index
        label = "S" // integer_to_character(new_label_index)
        do while (index_character_box(self%nonterminals, label) > 0) 
            new_label_index = new_label_index + 1
            label = "S" // integer_to_character(new_label_index)
        end do

        character_box%s = label
        self%nonterminals = [self%nonterminals, character_box]
        allocate(new_nonterminal_production%p(0))
        self%productions = [self%productions, new_nonterminal_production]
    end subroutine
    
    subroutine normalize_1(self)
        ! 1. New start terminal
        implicit none
        class(cfgrammar_type), intent(in out) ::self
      
        integer :: new_nonterminal_index
        type(tokens_type) :: production
        type(token_type) :: token

        call create_nonterminal(self, new_nonterminal_index)
        
        token%is_terminal = .false.
        token%index = self%initial_nonterminal
        production%tokens = [token]
        self%productions(new_nonterminal_index)%p = [self%productions(new_nonterminal_index)%p, production]
        self%initial_nonterminal = new_nonterminal_index
    end subroutine

    subroutine normalize_2(self)
        ! 2. Delete nonsingle terminals 
        ! add rules S_c -> c
        implicit none
        class(cfgrammar_type), intent(in out) ::self
        
        integer :: new_nonterminal_index, i, j, k
        type(tokens_type) :: production
        type(token_type), dimension(:), allocatable :: tokens
        type(token_type) :: token

        do i = 1, size(self%productions)
            do j = 1, size(self%productions(i)%p)
                tokens = self%productions(i)%p(j)%tokens
                if (size(tokens) > 1) then
                    do k = 1, size(tokens)
                        token = tokens(k)
                        if (token%is_terminal) then
                            call create_nonterminal(self, new_nonterminal_index)

                            production%tokens = [token]
                            self%productions(new_nonterminal_index)%p = [production]

                            token%is_terminal = .false.
                            token%index = new_nonterminal_index
                            self%productions(i)%p(j)%tokens(k) = token
                        end if
                    end do
                end if
            end do
        end do
    end subroutine

    subroutine normalize_3(self)
        ! 3. Delete long productions
        ! X_0 -> X_1 X_2 ... X_n transforms to X_0 -> S_1 X_3 ... X_n and S_1 -> X_1 X_2
        implicit none
        class(cfgrammar_type), intent(in out) ::self
        
        integer :: new_nonterminal_index, i, j, k
        type(tokens_type) :: production
        type(token_type), dimension(:), allocatable :: tokens, new_tokens
        type(token_type) :: token1, token2, new_token

        do i = 1, size(self%productions)
            do j = 1, size(self%productions(i)%p)
                tokens = self%productions(i)%p(j)%tokens
                do while (size(tokens) > 2)
                    allocate(new_tokens(0))

                    do k = 1, size(tokens) / 2
                        token1 = tokens(2*k - 1)
                        token2 = tokens(2*k)

                        call create_nonterminal(self, new_nonterminal_index)
                        production%tokens = [token1, token2]
                        self%productions(new_nonterminal_index)%p = [production]

                        new_token%is_terminal = .false.
                        new_token%index = new_nonterminal_index

                        new_tokens = [new_tokens, new_token]
                    end do
                    if (mod(size(tokens), 2) == 1) new_tokens = [new_tokens, tokens(size(tokens))]

                    tokens = new_tokens
                    deallocate(new_tokens)
                end do
                self%productions(i)%p(j)%tokens = tokens
            end do
        end do
    end subroutine

    subroutine normalize_4(self)
        ! 4. Delete eps productions 
        implicit none
        class(cfgrammar_type), intent(in out) ::self
        
        integer :: i, j
        type(tokens_type), dimension(:), allocatable :: production_without_eps
        type(tokens_type) :: new_tokens
        type(token_type), dimension(:), allocatable :: tokens
        logical, dimension(size(self%nonterminals)) :: eps_producing_nonterminals
        logical :: something_changes, is_eps_producing

        ! find eps producing nonterminals
        ! base
        eps_producing_nonterminals = .false.
        do i = 1, size(self%productions)
            allocate(production_without_eps(0))
            do j = 1, size(self%productions(i)%p)
                if (size(self%productions(i)%p(j)%tokens) == 0) then
                    eps_producing_nonterminals(i) = .true.   
                    cycle                 
                end if
                production_without_eps = [production_without_eps, self%productions(i)%p(j)]
            end do
            self%productions(i)%p = production_without_eps
            deallocate(production_without_eps)
        end do

        ! find transitive eps producing nonterminals
        something_changes = .true.
        do while (something_changes)    
            something_changes = .false.
            do i = 1, size(self%productions)
                if (eps_producing_nonterminals(i)) cycle
                do j = 1, size(self%productions(i)%p)
                    tokens = self%productions(i)%p(j)%tokens
                    ! if rule is A -> BC or A -> B, where B is non teerminal 
                    ! rules longer than 2 is deleted in normalize_3() step
                    ! rules A -> kC or A -> Bk is deleted in normalize_2() step
                    if (.not. tokens(1)%is_terminal) then
                        is_eps_producing = eps_producing_nonterminals(tokens(1)%index)
                        if (size(tokens) == 2) is_eps_producing = is_eps_producing .and. eps_producing_nonterminals(tokens(2)%index)
                        if (is_eps_producing) then
                            eps_producing_nonterminals(i) = .true.
                            something_changes = .true.
                            exit
                        end if
                    end if                    
                end do
            end do
        end do

        ! add new rules without eps producing rules
        do i = 1, size(self%productions)
            do j = 1, size(self%productions(i)%p)
                tokens = self%productions(i)%p(j)%tokens
                if (size(tokens) == 2) then
                    if (eps_producing_nonterminals(tokens(1)%index)) then
                        new_tokens%tokens = [tokens(2)]
                        self%productions(i)%p = [self%productions(i)%p, new_tokens]
                    end if
                    if (eps_producing_nonterminals(tokens(2)%index)) then
                        new_tokens%tokens = [tokens(1)]
                        self%productions(i)%p = [self%productions(i)%p, new_tokens]
                    end if
                end if
            end do
        end do    
        
        ! add _eps production to initial nonterminal if needed
        if (eps_producing_nonterminals(self%initial_nonterminal)) then
            if (allocated(new_tokens%tokens)) deallocate(new_tokens%tokens)
            allocate(new_tokens%tokens(0))
            self%productions(self%initial_nonterminal)%p = [self%productions(self%initial_nonterminal)%p, new_tokens]
        end if
    end subroutine

    subroutine normalize_5(self)
        ! 5. Delete chain productions
        ! if A -> B and B -> X_1 X_2 ... X_n then A -> X_1 X_2 ... X_n
        implicit none
        class(cfgrammar_type), intent(in out) ::self
        
        integer :: index, i, j, k, a, b

        type(production_type), dimension(:), allocatable :: new_productions_list
        type(tokens_type), dimension(:), allocatable :: production
        type(token_type), dimension(:), allocatable :: tokens
        logical, dimension(size(self%nonterminals), size(self%nonterminals)) :: is_chain_pair_exist 
        logical :: something_changes

        is_chain_pair_exist = .false.
        do i = 1, size(self%nonterminals)
            is_chain_pair_exist(i, i) = .true.
        end do
        
        something_changes = .true.
        do while (something_changes) 
            something_changes = .false.
            do a = 1, size(self%productions)
                do b = 1, size(self%productions)
                    if (is_chain_pair_exist(a, b)) then
                        do i = 1, size(self%productions(b)%p)
                            tokens = self%productions(b)%p(i)%tokens
                            if (size(tokens) == 1) then
                                if (.not. tokens(1)%is_terminal) then
                                    if (.not. is_chain_pair_exist(a, tokens(1)%index)) then
                                        something_changes = .true.
                                        is_chain_pair_exist(a, tokens(1)%index) = .true.
                                    end if
                                end if
                            end if
                        end do
                    end if
                end do
            end do
        end do

        allocate(new_productions_list(size(self%productions)))
        do a = 1, size(self%productions)
            allocate(production(0))
            do b = 1, size(self%productions)
                if (is_chain_pair_exist(a, b)) then
                    do i = 1, size(self%productions(b)%p)   
                        tokens = self%productions(b)%p(i)%tokens
                        if (size(tokens) /= 1) then 
                            production = [production, self%productions(b)%p(i)]
                        else if (tokens(1)%is_terminal) then
                            production = [production, self%productions(b)%p(i)]
                        end if
                    end do                 
                end if
            end do
            new_productions_list(a)%p = production
            deallocate(production)
        end do
        self%productions = new_productions_list
    end subroutine

    subroutine normalize(self)
        implicit none
        class(cfgrammar_type), intent(in out) ::self

        ! 1. New start terminal
        call normalize_1(self)

        ! 2. Delete nonsingle terminals 
        call normalize_2(self)

        ! 3. Delete long productions
        call normalize_3(self) 

        ! 4. Delete eps productions
        call normalize_4(self) 

        ! 5. Delete chain productions
        call normalize_5(self) 
    end subroutine

    function is_empty(self)
        implicit none
        class(cfgrammar_type), intent(in out) ::self
        logical :: is_empty

        integer :: i, j
        logical :: something_changes
        logical, dimension(:), allocatable :: productive_nonterminals
        type(token_type), dimension(:), allocatable :: tokens

        call self%normalize()
        allocate(productive_nonterminals(size(self%nonterminals)))

        ! find productive nonterminals
        ! base
        productive_nonterminals = .false.
        do i = 1, size(self%productions)
            do j = 1, size(self%productions(i)%p)
                if (size(self%productions(i)%p(j)%tokens) == 1) then
                    productive_nonterminals(i) = .true.  
                    exit          
                end if
            end do
        end do

        something_changes = .true.
        do while (something_changes)    
            something_changes = .false.
            do i = 1, size(self%productions)
                if (productive_nonterminals(i)) cycle
                do j = 1, size(self%productions(i)%p)
                    tokens = self%productions(i)%p(j)%tokens
                    
                    if (    size(tokens) == 2 .and. &
                            productive_nonterminals(tokens(1)%index) .and.  &
                            productive_nonterminals(tokens(2)%index) ) then
                        something_changes = .true.
                        productive_nonterminals(i) = .true.
                        exit
                    end if               
                end do
            end do
        end do
        
        print *, productive_nonterminals
        is_empty = .not. productive_nonterminals(self%initial_nonterminal)
    end function
        
    function build_cyk_table(self, str) result(table)
        implicit none
        class(cfgrammar_type), intent(in out) :: self
        character(*) :: str
        integer, dimension(:, :, :, :), allocatable :: table

        integer :: i, j, k, t, s, index, m, mid, index_left, index_right
        integer, dimension(len(str)) :: index_str
        type(token_type), dimension(:), allocatable :: tokens

        ! call self%normalize() todo
        allocate(table(size(self%nonterminals), len(str), len(str), 2))
        table = 0

        do i = 1, len(str)
            index_str(i) = index_character_box(self%terminals, str(i:i))
            if (index_str(i) == 0) then
                ! word not in language. return empty table?
            end if
        end do

        ! initialization
        ! fill diagonal
        do i = 1, size(self%productions)
            do j = 1, size(self%productions(i)%p)
                tokens = self%productions(i)%p(j)%tokens
                if (size(tokens) == 1) then
                    index = tokens(1)%index
                    do k = 1, size(index_str)
                        if (index_str(k) == index) then
                            ! table(i, k, k, 1) = k
                            table(i, k, k, 2) = j
                        end if
                    end do
                end if
            end do           
        end do

        ! dynamic
        do m = 2, size(index_str)
            do i = 1, size(index_str) - m + 1
                do j = m, size(index_str)
                    do mid = i, j-1
                        ! iterate in productions rules
                        do k = 1, size(self%productions)
                            do s = 1, size(self%productions(k)%p)
                                tokens = self%productions(k)%p(s)%tokens
                                if (size(tokens) == 2) then
                                    index_left = tokens(1)%index
                                    index_right = tokens(2)%index
                                    if (table(index_left, i, mid, 2) /= 0 .and.&
                                         table(index_right, mid + 1, j, 2) /= 0) then
                                        table(k, i, j, 1) = mid
                                        table(k, i, j, 2) = s
                                        exit
                                    end if
                                end if 
                            end do
                        end do 
                    end do
                end do
            end do
        end do
    end function

    subroutine print_cyk_table(grammar, cyk_table)
        implicit none
        class(cfgrammar_type) :: grammar
        integer, dimension(:, :, :, :), allocatable :: cyk_table

        integer, dimension(:), allocatable :: shp
        integer :: i, j, k
        
        shp = shape(cyk_table)

        do i = 1, shp(2)
            do j = 1, shp(3)
                do k = 1, shp(1)
                    if (cyk_table(k, i, j, 2) /= 0) then
                        write (*, '(a)', advance="no") grammar%nonterminals(k)%s // ","
                    end if 
                end do
                write (*, '(a)', advance="no") " | "
            end do
            print*
        end do

    end subroutine

    recursive subroutine print_parse_tree(grammar, cyk_table, x, y, z, level)
        implicit none
        class(cfgrammar_type) :: grammar
        integer, dimension(:, :, :, :), allocatable :: cyk_table
        integer :: x, y, z
        integer :: level

        integer, parameter :: TAB_SIZE = 4
        character(:), allocatable :: label
        integer :: mid, production_index
        type(token_type) :: left_token, right_token

        mid = cyk_table(x, y, z, 1)
        production_index = cyk_table(x, y, z, 2)

        if (production_index == 0) return

        label = grammar%nonterminals(x)%s

        print*, repeat(" ", level * TAB_SIZE) // label

        if (y == z) then
            label = grammar%terminals(grammar%productions(x)%p(production_index)%tokens(1)%index)%s
            print*, repeat(" ", (level + 1)*TAB_SIZE) // label
            return
        end if
        left_token = grammar%productions(x)%p(production_index)%tokens(1)
        right_token = grammar%productions(x)%p(production_index)%tokens(2)
        call print_parse_tree(grammar, cyk_table, left_token%index, y, mid, level + 1)      
        call print_parse_tree(grammar, cyk_table, right_token%index, mid + 1, z, level + 1)
    end subroutine
end module cfgrammar_module