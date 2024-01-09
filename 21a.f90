program aoc21a  
    implicit none
    
    type Coord
        integer :: y, x
    endtype

    character(len=150) :: line
    character(len=150), allocatable :: lines(:)
    integer :: status, i, k, m, pos, numSteps=64
    type(Coord) :: beg, dirs(4), next
    type(Coord), allocatable :: allPos(:), nextPos(:)

    i = 1
    allocate(lines(0))
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit
        ! print *, "LINE:", line

        pos = index(line, "S")
        if (pos > 0) then
            beg = Coord(i, pos)
            line = line(1:pos-1) // "." // trim(line(pos+1:))
        endif
        lines = [lines, line]

        i = i + 1
    enddo
    ! print *, "LINES:"
    ! do i=1, size(lines)
    !     print *, lines(i)
    ! enddo

    dirs = [Coord(1, 0), Coord(0, 1), Coord(-1, 0), Coord(0, -1)]
    allPos = [beg]
    do i=1, numSteps
        allocate(nextPos(0))

        do k=1, size(allPos)
            do m=1, size(dirs)
                next = allPos(k)
                next%y = next%y + dirs(m)%y
                next%x = next%x + dirs(m)%x

                if (lines(next%y)(next%x:next%x) == "." .and. (.not. any(nextPos(:)%y == next%y .and. nextPos(:)%x == next%x))) then
                    nextPos = [nextPos, next]
                endif
            enddo
        enddo

        call move_alloc(nextPos, allPos)
    enddo
    ! print *, "ALLPOS:", allPos
    print *, "PART 1:", size(allPos)

    ! print *, "MAP"
    ! do i=1, size(lines)
    !     line = ""
    !     do k=1, len_trim(lines(i))
    !         if (any(allPos(:)%y == i .and. allPos(:)%x == k)) then
    !             line = trim(line) // "O"
    !         else
    !             line = trim(line) // lines(i)(k:k)
    !         endif
    !     enddo
    !     print *, trim(line)
    ! enddo

end program aoc21a