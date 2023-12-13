program aoc10b
    implicit none

    type Coords
        integer:: y, x
    endtype

    character(len=200), allocatable :: pipesMap(:), origMap(:)
    character(len=200) :: line
    integer :: status, sPos, lineNum, i, queueIdx, newX, newY, k, total
    type(Coords) :: fromPos, curr
    type(Coords) :: allDirs(4) = [Coords(-1, 0), Coords(0, 1), Coords(1, 0), Coords(0, -1)]
    type(Coords), allocatable :: queue(:)

    lineNum = 1
    do while (.not. is_iostat_end(status))
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        sPos = index(line, "S")
        if (sPos /= 0) fromPos = Coords(lineNum, sPos)

        if (allocated(pipesMap)) then
            pipesMap = [pipesMap, line]
        else
            pipesMap = [line]
        endif
        lineNum = lineNum + 1
    enddo
    ! print *, "pipesMap:"
    ! do i = 1, size(pipesMap)
    !     print *, trim(pipesMap(i))
    ! enddo
    ! print *, "S:", fromPos

    origMap = pipesMap  ! Save one copy with pipes intact
    call followPipes();  ! Basically, Part 1, but with pipe marking

    ! Pad around edges for outer space filling, later
    line = repeat(".", len_trim(line))
    pipesMap = [line, pipesMap, line]
    origMap = [line, origMap, line]
    do i = 1, size(pipesMap)
        pipesMap(i) = '.' // trim(pipesMap(i)) // '.'
        origMap(i) = '.' // trim(origMap(i)) // '.'
    enddo
    ! print *, "PADDING"
    ! do i = 1, size(pipesMap)
    !     print *, pipesMap(i)
    ! enddo
    ! ! do i = 1, size(origMap)
    ! !     print *, origMap(i)
    ! ! enddo
    
    ! Fill outsides with blanks.
    queue = [Coords(1, 1)]
    queueIdx = 1
    do while (queueIdx <= size(queue))
        curr = queue(queueIdx)
        queueIdx = queueIdx + 1
        ! print *, "CURR:", queueIdx, curr

        if (pipesMap(curr%y)(curr%x:curr%x) /= '~') then
            pipesMap(curr%y)(curr%x:curr%x) = '~'
            origMap(curr%y)(curr%x:curr%x) = '~'

            do i = 1, size(allDirs)
                newY = curr%y + allDirs(i)%y
                newX = curr%x + allDirs(i)%x
                
                if (newY > 0 .and. newY <= size(pipesMap) .and. &
                    newX > 0 .and. newX <= len_trim(pipesMap(1))) then
                        if (pipesMap(newY)(newX:newX) /= '~' .and. pipesMap(newY)(newX:newX) /= '#') then
                            queue = [queue, Coords(newY, newX)]
                        endif
                endif
            enddo
        endif
    enddo
    ! print *, "OUTER FILL"
    ! do i = 1, size(pipesMap)
    !     print *, pipesMap(i)
    ! enddo
    ! ! do i = 1, size(origMap)
    ! !     print *, origMap(i)
    ! ! enddo

    deallocate(queue)
    do i = 1, size(pipesMap)
        do k = 1, len_trim(pipesMap(i))
            if (pipesMap(i)(k:k) /= "~" .and. pipesMap(i)(k:k) /= "#") then
                pipesMap(i)(k:k) = "?"
                if (allocated(queue)) then
                    queue = [queue, Coords(i, k)]
                else
                    queue = [Coords(i, k)]
                endif
            endif
        enddo
    enddo
    ! print *, "LEFTOVERS", queue
    ! do i = 1, size(pipesMap)
    !     print *, pipesMap(i)
    ! enddo

    total = 0
    do i = 1, size(queue)
        if (isInside(queue(i)%y, queue(i)%x)) total = total + 1
    enddo
    ! print *, "FINAL", queue
    ! do i = 1, size(pipesMap)
    !     print *, pipesMap(i)
    ! enddo
    ! do i = 1, size(origMap)
    !     print *, origMap(i)
    ! enddo

    print *, "PART 2:", total

contains
    logical function isInside(y, x)
        integer, intent(in) :: y, x
        integer :: currX, numWalls, turns(2)
        character :: currChar

        isInside = .false.
        numWalls = 0
        turns = 0
        currX = x - 1
        do while (currX > 0)
            currChar = pipesMap(y)(currX:currX)
            if (currChar == '#') then
                currChar = origMap(y)(currX:currX)

                if (currChar == '|') then
                    numWalls = numWalls + 1
                else if (currChar == 'F') then
                    turns(1) = turns(1) + 1
                else if (currChar == "7") then
                    turns(1) = turns(1) - 1
                else if (currChar == "L") then
                    turns(2) = turns(2) + 1
                else if (currChar == "J") then
                    turns(2) = turns(2) - 1
                endif 
            else if (currChar == '~') then
                exit
            endif
            currX = currX - 1
        enddo
        ! print *, y, x, "WALLS:", numWalls, "TURNS:", turns
        numWalls = numWalls + ((abs(turns(1)) + abs(turns(2))) / 2)

        if (mod(numWalls, 2) /= 0) then
            pipesMap(y)(x:x) = 'I'
            origMap(y)(x:x) = 'I'
            isInside = .true.
        else
            pipesMap(y)(x:x) = 'O'
        endif
    endfunction

    type(Coords) function findStartDirs(startPos)
        type(Coords), intent(in) :: startPos
        character(len=2) :: exits
        exits = ""

        ! print *, "TESTING:", pipesMap(startPos%y-1)(startPos%x:startPos%x), pipesMap(startPos%y+1)(startPos%x:startPos%x), &
        !     pipesMap(startPos%y)(startPos%x-1:startPos%x-1), pipesMap(startPos%y)(startPos%x+1:startPos%x+1), &
        !     startPos%y < size(pipesMap), (pipesMap(startPos%y+1)(startPos%x:startPos%x) == "|" &
        !     .or. pipesMap(startPos%y+1)(startPos%x:startPos%x) == "L" &
        !     .or. pipesMap(startPos%y+1)(startPos%x:startPos%x) == "J")

        if (startPos%y > 1 .and. (pipesMap(startPos%y-1)(startPos%x:startPos%x) == "|" &
                .or. pipesMap(startPos%y-1)(startPos%x:startPos%x) == "F" &
                .or. pipesMap(startPos%y-1)(startPos%x:startPos%x) == "7")) then
                    findStartDirs = Coords(startPos%y-1, startPos%x)
                    exits = trim(exits) // "N"
        endif
        if (startPos%y < size(pipesMap) .and. (pipesMap(startPos%y+1)(startPos%x:startPos%x) == "|" &
            .or. pipesMap(startPos%y+1)(startPos%x:startPos%x) == "L" &
            .or. pipesMap(startPos%y+1)(startPos%x:startPos%x) == "J")) then
                    findStartDirs = Coords(startPos%y+1, startPos%x)
                    exits = trim(exits) // "S"
        endif
        if (startPos%x > 1 .and. (pipesMap(startPos%y)(startPos%x-1:startPos%x-1) == "-" &
            .or. pipesMap(startPos%y)(startPos%x-1:startPos%x-1) == "F" &
            .or. pipesMap(startPos%y)(startPos%x-1:startPos%x-1) == "L")) then
                    findStartDirs = Coords(startPos%y, startPos%x-1)
                    exits = trim(exits) // "W"
        endif
        if (startPos%x < len_trim(pipesMap(1)) .and. (pipesMap(startPos%y)(startPos%x+1:startPos%x+1) == "-" &
            .or. pipesMap(startPos%y)(startPos%x+1:startPos%x+1) == "7" &
            .or. pipesMap(startPos%y)(startPos%x+1:startPos%x+1) == "J")) then
                    findStartDirs = Coords(startPos%y, startPos%x+1)
                    exits = trim(exits) // "E"
        endif

        ! Replace the 'S' with the proper symbol
        if (exits == "NS") then
            origMap(startPos%y)(startPos%x:startPos%x) = "|"
        else if (exits == "NW") then
            origMap(startPos%y)(startPos%x:startPos%x) = "J"
        else if (exits == "NE") then
            origMap(startPos%y)(startPos%x:startPos%x) = "L"
        else if (exits == "SW") then
            origMap(startPos%y)(startPos%x:startPos%x) = "7"
        else if (exits == "SE") then
            origMap(startPos%y)(startPos%x:startPos%x) = "F"
        endif
        ! print *, "EXITS:", exits
    endfunction

    subroutine followPipes()
        character :: nextLetter
        integer :: stepNum
        type(coords) :: nextPos, newPos

        nextPos = findStartDirs(fromPos)
        stepNum = 0
        do
            stepNum = stepNum + 1
            nextLetter = pipesMap(nextPos%y)(nextPos%x:nextPos%x)
            pipesMap(nextPos%y)(nextPos%x:nextPos%x) = '#'
    
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
        ! print *, "PART 1:", stepNum / 2  ! Confirm
    endsubroutine
end program aoc10b