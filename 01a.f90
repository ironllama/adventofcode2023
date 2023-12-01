program aoc01a
    implicit none
    
    integer :: status, i, lineTotal, total
    character(len=100) :: line
    character(len=10) :: lineNums
    character(len=1), dimension(9) :: numStr = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

    status = 0
    total = 0

    do while (status == 0)
        lineTotal = 0
        lineNums = ''

        read (*, '(a)', iostat=status) line
        if (status /= 0) exit
        ! print *, line

        do i = 1,len(line)
            if (any(numStr == line(i:i))) then
                lineNums = trim(lineNums) // line(i:i)
                exit
            end if
        end do
        do i = len(line), 1, -1
            if (any(numStr == line(i:i))) then
                lineNums = trim(lineNums) // line(i:i)
                exit
            end if
        end do

        read (lineNums, *) lineTotal
        ! print *, lineNums, lineTotal
        total = total + lineTotal
    end do
    print *, total
end program aoc01a
