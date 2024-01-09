program aoc21b
    implicit none
    
    type Coord
        integer :: y, x
    endtype

    character(len=150) :: line
    character(len=150), allocatable :: lines(:)
    integer :: status, i, pos, steps, num, fLen, fMid
    integer(kind=8) :: w, total=0
    type(Coord) :: beg

    i = 1
    allocate(lines(0))
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit
        ! print *, "LINE:[", trim(line), "]"

        pos = index(trim(line), "S")
        if (pos > 0) then
            beg = Coord(i, pos)
            line = line(1:pos-1) // "." // trim(line(pos+1:))
        endif
        lines = [lines, trim(line)]

        i = i + 1
    enddo
    ! print *, "LINES:"
    ! do i=1, size(lines)
    !     print *, lines(i)
    ! enddo

    ! Most of this was done offline w/ calculator. But it started with the realization that the
    ! 26501365 steps was somehow significant. It is compatible with an whole number of "fields" to
    ! one side of the S. Particularly, 26501365 - 65 (steps to edge) = 26501300.
    ! Then 26501300 / 131 steps per field width = 202300 fields -- a whole number! Coincidence? Nah.
    ! So, the total area would be 202300 fields to the left and to the bottom. However, it is a
    ! diamond instead of a square, so to get the area, the calculation is a bit different. You can
    ! tilt the diamond 45deg to one side and then see that the inner area of the diamond contains
    ! a number of whole fields that correspond to (w*w) + ((w-1)*(w-1)). From looking at the way
    ! that fields butt up against each other, it seems some fields will be the odd step pattern and
    ! some will be even step pattern. So the w will be the odd step pattern and the w-1 will be in
    ! the even step pattern. First, we get the number of valid spots for both the even and odd steps:
    fLen = len_trim(lines(1))
    fMid = beg%x

    steps = 26501365
    w = int8((steps - fMid + 1) / fLen)  ! Remove beg square and steps to edge. Divide by field width.

    num = getValidSpots(beg, fLen+1)  ! Even steps
    total = total + ((w * w) * int8(num))
    print *, "EVEN WHOLES:", fLen+1, num, total

    num = getValidSpots(beg, fLen)  ! Odd steps
    total = total + (((w-1) * (w-1)) * int8(num))
    print *, "ODD WHOLES:", fLen, num, total

    ! Then we get the boxes along the UPPER RIGHT border of the diamond, which consists of partials.
    ! There are w-1 number of large partials and w number of small corners
    num = getValidSpots(Coord(fLen, 1), fLen+fMid-2)  ! Large partials from the lower left corner.
    total = total + ((w-1) * int8(num))
    print *, "UPPER RIGHT LARGE:", fLen+fMid-2, num, total

    num = getValidSpots(Coord(1, fLen), fMid-2)  ! -2 so keep evens, but 1 layer back from border
    total = total + (w * int8(num))
    print *, "UPPER RIGHT SMALL:", fMid-2, num, total

    ! Then we get the boxes along the LOWER RIGHT border of the diamond, which consists of partials.
    num = getValidSpots(Coord(1, 1), fLen+fMid-2)  ! Large partials from the upper left corner.
    total = total + ((w-1) * int8(num))
    print *, "LOWER RIGHT LARGE:", fLen+fMid-2, num, total

    num = getValidSpots(Coord(fLen, fLen), fMid-2)
    total = total + (w * int8(num))
    print *, "LOWER RIGHT SMALL:", fMid-2, num, total

    ! Then we get the boxes along the LOWER LEFT border of the diamond, which consists of partials.
    num = getValidSpots(Coord(1, fLen), fLen+fMid-2)  ! Large partials from the upper right corner.
    total = total + ((w-1) * int8(num))
    print *, "LOWER LEFT LARGE:", fLen+fMid-2, num, total

    num = getValidSpots(Coord(fLen, 1), fMid-2)
    total = total + (w * int8(num))
    print *, "LOWER LEFT SMALL:", fMid-2, num, total

    ! Then we get the boxes along the UPPER LEFT border of the diamond, which consists of partials.
    num = getValidSpots(Coord(fLen, fLen), fLen+fMid-2)  ! Large partials from the lower right corner.
    total = total + ((w-1) * int8(num))
    print *, "UPPER LEFT LARGE:", fLen+fMid-2, num, total

    num = getValidSpots(Coord(1, 1), fMid-2)
    total = total + (w * int8(num))
    print *, "UPPER LEFT SMALL:", fMid-2, num, total

    ! Finally, we deal with the corner pieces. Even though these are odd step fields, we start with
    ! and even since it would normally end with an O for odd (ie. the starting point is opposite)
    num = getValidSpots(Coord(fLen, fMid), fLen-1)
    total = total + int8(num)
    print *, "TOP - FROM BOTTOM:", fLen-1, num, total

    num = getValidSpots(Coord(fMid, 1), fLen-1)
    total = total + int8(num)
    print *, "RIGHT - FROM LEFT:", fLen-1, num, total

    num = getValidSpots(Coord(1, fMid), fLen-1)
    total = total + int8(num)
    print *, "BOTTOM - FROM TOP:", fLen-1, num, total

    num = getValidSpots(Coord(fMid, fLen), fLen-1)
    total = total + int8(num)
    print *, "LEFT - FROM RIGHT:", fLen-1, num, total

    print *, "PART 2:", total

contains
    integer function getValidSpots(beg, steps)
        type(Coord), intent(in) :: beg
        integer, intent(in) :: steps
        type(Coord), allocatable :: allPos(:), nextPos(:)
        type(Coord) :: next, dirs(4) = [Coord(1, 0), Coord(0, 1), Coord(-1, 0), Coord(0, -1)]
        integer i, k, m

        allPos = [beg]
        do i=1, steps
            allocate(nextPos(0))

            do k=1, size(allPos)
                do m=1, size(dirs)
                    next = allPos(k)
                    next%y = next%y + dirs(m)%y
                    next%x = next%x + dirs(m)%x

                    if (lines(next%y)(next%x:next%x) == "." &
                            .and. (.not. any(nextPos(:)%y == next%y .and. nextPos(:)%x == next%x))) then
                        nextPos = [nextPos, next]
                    endif
                enddo
            enddo

            call move_alloc(nextPos, allPos)
        enddo
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

        getValidSpots = size(allPos)
        deallocate(allPos)
    endfunction
end program aoc21b