program aoc13b
    implicit none
    
    character(len=30), allocatable :: lines(:), origLines(:), tempLines(:)
    character(len=30) :: line
    integer :: status, total, hPrev, vPrev, i, k, pos

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
            origLines = lines  ! Otherwise, we can undo the rotate

            vPrev = 0
            hPrev = checkReflection(lines, 0)
            if (hPrev == 0) then
                call rotateArrayClockwise(lines)
                vPrev = checkReflection(lines, 0)
            endif
            ! print *, hPrev, vPrev, "LINES:"
            ! do i = 1, size(lines)
            !     print *, lines(i)
            ! enddo
            deallocate(lines)

            outer: do i = 1, size(origLines)
                do k = 1, len_trim(origLines(i))
                    tempLines = origLines

                    if (tempLines(i)(k:k) == '.') then
                        tempLines(i)(k:k) = '#'
                    else
                        tempLines(i)(k:k) = '.'
                    endif

                    pos = checkReflection(tempLines, hPrev) * 100
                    if (pos == 0) then
                        call rotateArrayClockwise(tempLines)
                        pos = checkReflection(tempLines, vPrev)
                    endif
                    deallocate(tempLines)

                    if (pos > 0) exit outer
                enddo
            enddo outer

            ! print *, "TOTAL:", total, " POS:", pos, hPrev, vPrev, "LINE:"
            ! do i = 1, size(origLines)
            !     print *, origLines(i)
            ! enddo
            total = total + pos
            deallocate(origLines)
        endif
    enddo

    print *, "PART 2:", total

contains
    integer function checkReflection(lines, previous)
        character(len=*), allocatable, intent(in) :: lines(:)
        integer, intent(in) :: previous
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

            if (match .and. split /= previous) then
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
end program aoc13b