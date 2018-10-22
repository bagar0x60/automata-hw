module parse_tools_module
implicit none

    type character_box_type
        character(:), allocatable :: s
    end type

contains 
    function index_character_box(array, item) result(index)
    implicit none
    type(character_box_type), dimension(:), allocatable :: array
    character(*) :: item
    integer :: index

        do index = 1, size(array)
            if (array(index)%s == item) return
        end do 
        index = 0
    end function

    subroutine read_line(unit, line, ier)
    implicit none
    integer, intent(in) :: unit
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: ier
    
       integer, parameter :: buffer_size = 1024
       character(len=buffer_size) :: buffer
       integer :: last
       integer :: isize
    
       line = ''
       ier = 0
    
       READ_LOOP: do
          read(unit, iostat=ier, fmt='(a)', advance='no', size=isize) buffer
          if (isize .gt. 0) line = line//buffer(:isize)
          
          ! if hit EOR reading is complete unless backslash ends the line
          if (is_iostat_eor(ier)) then
             last=len(line)
             if(last .ne. 0)then
                if (line(last:last) .eq. '\')then
                   line = line(:last-1)
                   cycle READ_LOOP
                endif
             endif
             ! hitting end of record is not an error for this routine
             ier = 0
             exit READ_LOOP
         ! end of file or error
         elseif(ier .ne. 0) then
            exit READ_LOOP
         endif
       enddo READ_LOOP    
    end subroutine read_line


    function split(str, separator, without_empty_substrings)
    implicit none
    character(:), allocatable :: str
    character(*) :: separator
    logical, optional :: without_empty_substrings
    type(character_box_type), dimension(:), allocatable :: split
        
        integer :: pos 
        type(character_box_type) :: substring
        logical :: without_empty 

        if (.not. present(without_empty_substrings)) then 
            without_empty = .true.
        else
            without_empty = without_empty_substrings
        end if

        allocate(split(0))
        pos = index(str, separator)
        do while (pos > 0) 
            substring%s = str(:pos-1)
            if (.not. (without_empty .and. (len(substring%s) == 0)) ) split = [split, substring]
            str = str(pos + len(separator):)
            pos = index(str, separator)

            !print*, substring%s, pos, str, len(str), len(substring%s)
            !read(*,*) pos
        end do

        if (len(str) > 0) then
            substring%s = str
            split = [split, substring]
        end if
    end function
end module 