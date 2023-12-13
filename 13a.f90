program aoc13a
    implicit none
    
    character(len=30), allocatable :: lines(:)
    character(len=30) :: line
    integer :: status, pos, total

    total = 0
    do while(.not. is_iostat_end(status))
        read (*, '(a)', iostat=status) line

        if (len_trim(line) > 0 .and. .not. is_iostat_end(status)) then
            if (allocated(lines)) then
                lines = [lines, line]
            else
                lines = [line]
            endif
        else  ! Blank line -- let's process the last block!
            pos = checkReflection(lines) * 100
            ! print *, "HORI:", pos
            ! do i = 1, size(lines)
            !     print *, lines(i)
            ! enddo

            if (pos == 0) then
                call rotateArrayClockwise(lines)
                pos = checkReflection(lines)
                ! print *, "VERT:", pos
                ! do i = 1, size(lines)
                !     print *, lines(i)
                ! enddo
            endif

            total = total + pos
            deallocate(lines)
        endif
    enddo

    print *, "PART 1:", total

contains
    integer function checkReflection(lines)
        character(len=*), allocatable, intent(in) :: lines(:)
        integer :: split, top, bottom
        logical :: match

        checkReflection = 0
        do split = 1, size(lines) - 1
            top = split
            bottom = split + 1
            match = .true.
            do while(top > 0 .and. bottom <= size(lines))
                if (lines(top) /= lines(bottom)) then
                    match = .false.
                    exit
                endif
                top = top - 1
                bottom = bottom + 1
            enddo

            if (match) then
                checkReflection = split
                exit
            endif
        enddo
    endfunction

    subroutine rotateArrayClockwise(inArray)
        implicit none
        character(len=*), allocatable, intent(inout) :: inArray(:)
        character(len=30), allocatable :: tempArray(:)
        integer :: rows, cols, i, j
      
        rows = size(inArray)
        cols = len_trim(inArray(1))
        allocate(tempArray(cols))
        do i = 1, size(tempArray)
            tempArray(i) = repeat(" ", rows);
        enddo
      
        do i = 1, rows
           do j = 1, cols
              tempArray(j)(rows - i + 1:rows - i + 1) = inArray(i)(j:j)
           end do
        end do
        call move_alloc(tempArray, inArray)
    endsubroutine
end program aoc13a