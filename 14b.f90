program aoc14b
    implicit none

    type History
        character(len=150), allocatable :: lines(:)
        integer :: pos
    endtype
    
    character(len=150), allocatable :: lines(:)
    character(len=150) :: line
    integer :: status, r, c, total, i, k, pMatch, bRepeat, pRepeat, nRepeat, maxRepeat, cycleSize
    type(History), allocatable :: allHistory(:)
    type(History) :: tempHistory

    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit
        if (allocated(lines)) then
            lines = [lines, line]
        else
            lines = [line]
        endif
    enddo
    ! print *, "LINES:"
    ! do r = 1, size(lines)
    !     print *, trim(lines(r))
    ! enddo

    pRepeat = 0
    bRepeat = 0
    nRepeat = 0
    maxRepeat = 3
    do i = 0, 1000000000
        call runCycle(lines);
        tempHistory = History(lines, i)

        pMatch = 0
        if (allocated(allHistory)) then
            do k = 1, size(allHistory)
                if (all(allHistory(k)%lines == lines)) then
                    pMatch = k
                    exit
                endif
            enddo
        endif
        if (pMatch /= 0) then
            if (pRepeat == 0 .or. pRepeat == i - 1) then
                if (bRepeat == 0) bRepeat = allHistory(pMatch)%pos

                nRepeat = nRepeat + 1
                if (nRepeat == maxRepeat) then
                    cycleSize = ((i - maxRepeat) - bRepeat) + 1
                    exit
                endif
            else
                nRepeat = 0
                bRepeat = 0
            endif
            pRepeat = i
        else
            if (allocated(allHistory)) then
                allHistory = [allHistory, tempHistory]
            else
                allHistory = [tempHistory]
            endif
        endif

        ! print *, "LINES:"
        ! total = 0
        ! do r = 1, size(lines)
        !     print *, trim(lines(r))
        !     do c = 1, len_trim(lines(r))
        !         if (lines(r)(c:c) == "O") total = total + (size(lines) - r + 1)
        !     enddo
        ! enddo
        ! print *, "TOTAL: ", total
    enddo

    i = mod(1000000000 - (bRepeat + 1), cycleSize)
    lines = allHistory(i + bRepeat + 1)%lines
    total = 0
    do r = 1, size(lines)
        do c = 1, len_trim(lines(r))
            if (lines(r)(c:c) == "O") total = total + (size(lines) - r + 1)
        enddo
    enddo

    print *, "PART 2:", total

contains
    subroutine runCycle(inArray)
        implicit none
        character(len=*), allocatable, intent(inout) :: inArray(:)

        call slideRocks(inArray)
        call rotateArrayClockwise(inArray)
        call slideRocks(inArray)
        call rotateArrayClockwise(inArray)
        call slideRocks(inArray)
        call rotateArrayClockwise(inArray)
        call slideRocks(inArray)

        call rotateArrayClockwise(inArray)
    endsubroutine

    subroutine slideRocks(inArray)
        implicit none
        character(len=*), allocatable, intent(inout) :: inArray(:)
        integer :: r, c, bottom

        do c = 1, len_trim(inArray(1))
            bottom = 1
            do r = 1, size(inArray)
                if (inArray(r)(c:c) == "O") then
                    if (r > bottom) then
                        inArray(bottom)(c:c) = "O"
                        inArray(r)(c:c) = "."
                    endif
                    bottom = bottom + 1
                else if (inArray(r)(c:c) == "#") then
                    bottom = r + 1
                endif
            enddo
        enddo
    endsubroutine

    subroutine rotateArrayClockwise(inArray)
        implicit none
        character(len=*), allocatable, intent(inout) :: inArray(:)
        character(len=150), allocatable :: tempArray(:)
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
end program aoc14b