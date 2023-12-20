program aoc18b
    implicit none
    
    type Coords
        integer :: y,x
    endtype

    character(len=20) :: line
    character :: dir
    character(len=10) :: hexStr
    integer :: status, amt, i, total
    integer(kind=8) :: shoelace
    type(Coords) :: curr, next

    shoelace = 0
    curr = Coords(1, 1)
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        read (line, *) dir, amt, hexStr
        dir = hexStr(8:8)
        hexStr = hexStr(3:7)
        read (hexStr, '(z5)') amt
        ! print *, "LINE: ", dir, amt, hexStr

        do i = 1, amt
            next = curr
            select case (dir)
                case ('0')
                    next%x = next%x + 1
                case ('2')
                    next%x = next%x - 1
                case ('1')
                    next%y = next%y + 1
                case ('3')
                    next%y = next%y - 1
            endselect

            shoelace = shoelace + ((curr%x * next%y) - (curr%y * next%x))

            curr = next
            total = total + 1
        enddo
    enddo
    shoelace = abs(shoelace) / 2  ! Absolute makes counter/clockwise irrelevant.

    ! Also use Pick's Theorem to include the outer edge of the borders.
    ! Points are centers of 1x1 squares, so we miss all corners by 0.5. So, we add 4 * 0.5, or 2
    shoelace = shoelace + (total / 2) - 1 + 2

    print *, "PART 2:", shoelace
end program aoc18b