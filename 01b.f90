program aoc01b
    implicit none
    
    integer :: status, i, linePosNum, linePosWord, lineTotal, total
    integer, parameter :: lineMax = 100
    character(len=lineMax) :: line
    character(len=10) :: lineNums, oneNumStr
    character(len=1), dimension(9) :: numStr = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
    character(len=5), dimension(9) :: wordStr = ["  one", "  two", "three", " four", " five", "  six", "seven", "eight", " nine"]
    integer, dimension(9) :: numPos

    status = 0
    total = 0

    do while (status == 0)
        lineTotal = 0
        lineNums = ''

        read (*, '(a)', iostat=status) line
        if (status /= 0) exit
        ! print *, line

        ! For each line, find the minimum positional occurance of each num
        numPos = lineMax 
        do i = 1,size(numStr)
            linePosNum = index(trim(line), numStr(i));
            if (linePosNum == 0) linePosNum = lineMax

            linePosWord = index(trim(line), trim(adjustl(wordStr(i))));
            if (linePosWord == 0) linePosWord = lineMax

            numPos(i) = min(linePosNum, linePosWord)
        end do
        ! print *, "L:", numPos
        write (lineNums, '(i0)') minloc(numPos)

        ! For each line, find the maximum positional occurance of each num
        numPos = 0
        do i = 1,size(numStr)
            linePosNum = index(trim(line), numStr(i), back=.true.);

            linePosWord = index(trim(line), trim(adjustl(wordStr(i))), back=.true.);

            numPos(i) = max(linePosNum, linePosWord)
        end do
        ! print *, "R:", numPos
        write (oneNumStr, '(i0)') maxloc(numPos)
        lineNums = trim(lineNums) // trim(oneNumStr)

        read (lineNums, *) lineTotal
        total = total + lineTotal
        ! print *, line, lineNums, lineTotal, total
    end do
    print *, total
end program
