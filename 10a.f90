program aoc10a
    implicit none

    type Coords
        integer:: y, x
    endtype

    character(len=200), allocatable :: lines(:)    
    character(len=200) :: line
    character :: nextLetter
    integer :: status, sPos, lineNum, stepNum
    type(coords) :: fromPos, nextPos, newPos

    lineNum = 1
    do while (.not. is_iostat_end(status))
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        sPos = index(line, "S")
        if (sPos /= 0) fromPos = Coords(lineNum, sPos)

        if (allocated(lines)) then
            lines = [lines, line]
        else
            lines = [line]
        endif
        lineNum = lineNum + 1
    enddo
    ! print *, "LINES:"
    ! do i = 1, size(lines)
    !     print *, trim(lines(i))
    ! enddo
    ! print *, "S:", fromPos

    nextPos = findStartDirs(fromPos)
    stepNum = 0
    do
        stepNum = stepNum + 1
        nextLetter = lines(nextPos%y)(nextPos%x:nextPos%x)

        newPos = nextPos
        if (nextLetter == 'S') then
            exit
        else if (nextLetter == "|") then
            if (newPos%y > fromPos%y) then
                newPos%y = newPos%y + 1
            else
                newPos%y = newPos%y - 1
            endif
        else if (nextLetter == "-") then
            if (newPos%x > fromPos%x) then
                newPos%x = newPos%x + 1
            else
                newPos%x = newPos%x - 1
            endif
        else if (nextLetter == "L") then
            if (newPos%y > fromPos%y) then
                newPos%x = newPos%x + 1
            else
                newPos%y = newPos%y - 1
            endif
        else if (nextLetter == "J") then
            if (newPos%y > fromPos%y) then
                newPos%x = newPos%x - 1
            else
                newPos%y = newPos%y - 1
            endif
        else if (nextLetter == "7") then
            if (newPos%y < fromPos%y) then
                newPos%x = newPos%x - 1
            else
                newPos%y = newPos%y + 1
            endif
        else if (nextLetter == "F") then
            if (newPos%y < fromPos%y) then
                newPos%x = newPos%x + 1
            else
                newPos%y = newPos%y + 1
            endif
        endif

        fromPos = nextPos
        nextPos = newPos
    enddo

    print *, "PART 1:", stepNum / 2

contains

    type(Coords) function findStartDirs(startPos)
        type(Coords), intent(in) :: startPos
        if (startPos%y > 1 .and. (lines(startPos%y-1)(startPos%x:startPos%x) == "|" &
                .or. lines(startPos%y-1)(startPos%x:startPos%x) == "F" &
                .or. lines(startPos%y-1)(startPos%x:startPos%x) == "7")) then
                    findStartDirs = Coords(startPos%y-1, startPos%x)
        else if (startPos%y < size(lines) .and. (lines(startPos%y+1)(startPos%x:startPos%x) == "|" &
            .or. lines(startPos%y+1)(startPos%x:startPos%x) == "L" &
            .or. lines(startPos%y+1)(startPos%x:startPos%x) == "J")) then
                    findStartDirs = Coords(startPos%y+1, startPos%x)
        else if (startPos%x > 1 .and. (lines(startPos%y)(startPos%x-1:startPos%x-1) == "-" &
            .or. lines(startPos%y)(startPos%x-1:startPos%x-1) == "F" &
            .or. lines(startPos%y)(startPos%x-1:startPos%x-1) == "L")) then
                    findStartDirs = Coords(startPos%y, startPos%x-1)
        else if (startPos%x > 1 .and. (lines(startPos%y)(startPos%x+1:startPos%x+1) == "-" &
            .or. lines(startPos%y)(startPos%x+1:startPos%x+1) == "7" &
            .or. lines(startPos%y)(startPos%x+1:startPos%x+1) == "J")) then
                    findStartDirs = Coords(startPos%y, startPos%x+1)
        endif
    endfunction
end program aoc10a
