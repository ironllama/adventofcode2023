program aoc18a
    implicit none
    
    type Coords
        integer :: y,x
    endtype

    character(len=20) :: line
    character :: dir
    character(len=10) :: hexStr
    character, allocatable :: grid(:,:), tempGrid(:,:)
    integer :: status, amt, i, k, m, qPtr, total
    type(Coords), allocatable :: queue(:)
    type(Coords) :: curr, next, min, max, mid, dirs(4)
    logical :: found

    curr = Coords(1, 1)
    min = Coords(999999, 999999)
    max = Coords(0, 0)
    queue = [curr]
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        read (line, *) dir, amt, hexStr
        ! print *, "LINE: ", dir, amt, hexStr

        do i = 1, amt
            ! nextX = currX
            ! nextY = currY
            next = curr
            select case (dir)
                case ('R')
                    next%x = next%x + 1
                    if (next%x < min%x) min%x = next%x
                    if (next%x > max%x) max%x = next%x
                case ('L')
                    next%x = next%x - 1
                    if (next%x < min%x) min%x = next%x
                    if (next%x > max%x) max%x = next%x
                case ('D')
                    next%y = next%y + 1
                    if (next%y < min%y) min%y = next%y
                    if (next%y > max%y) max%y = next%y
                case ('U')
                    next%y = next%y - 1
                    if (next%y < min%y) min%y = next%y
                    if (next%y > max%y) max%y = next%y
            endselect

            queue = [queue, next]
            curr = next
            total = total + 1
        enddo
    enddo
    ! print *, "Y:", maxY, minY, "X:", maxX, minX, "GAPS:", abs(maxY-minY), abs(maxX-minX)
    ! print *, "QUEUE:"
    ! do i = 1, size(queue)
    !     print *, queue(i)
    ! enddo

    ! Create the grid and borders
    do i = 1, abs(max%y-min%y)+1
        if (allocated(grid)) then
            allocate(tempGrid(i,1:abs(max%x-min%x)+1))
            tempGrid(1:i - 1,:) = grid
            call move_alloc(tempGrid, grid)
        else
            allocate(grid(i,1:abs(max%x-min%x)+1))
        endif

        do k = 1, abs(max%x-min%x)+1
            found = .false.
            do m = 1, size(queue)
                if (queue(m)%y == i + min%y-1 .and. queue(m)%x == k + min%x-1) found = .true.
            enddo
            if (found) then
                grid(i,k) = '#'
            else
                grid(i,k) = '.'
            endif
        enddo
    enddo
    ! print *, "BORDERS"
    ! do i = 1, size(grid, 1)
    !     print *, grid(i,:)
    ! enddo

    ! Flood fill the grid
    mid%y = ((abs(max%y - min%y) + 1) / 2) + 1
    mid%x = ((abs(max%x - min%x) + 1) / 2) + 1
    ! print *, "MID:", midY, midX

    dirs = [Coords(1, 0), Coords(0, 1), Coords(-1, 0), Coords(0, -1)]
    queue = [Coords(mid%y, mid%x)]
    qPtr = 1
    do while (qPtr <= size(queue))
        curr = queue(qPtr)
        qPtr = qPtr + 1
        ! print *, "CURR:", curr
        ! qPtr = qPtr + 1

        do i = 1, size(dirs)
            next = curr
            next%y = next%y + dirs(i)%y
            next%x = next%x + dirs(i)%x
            ! print *, "NEXT:", next

            if (grid(next%y, next%x) == ".") then
                grid(next%y, next%x) = '#'
                queue = [queue, next]
                total = total + 1
            endif
        enddo
    enddo
    ! print *, "FILLED"
    ! do i = 1, size(grid, 1)
    !     print *, grid(i,:)
    ! enddo

    print *, "PART 1:", total
end program aoc18a