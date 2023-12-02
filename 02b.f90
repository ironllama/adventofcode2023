program aoc02b
    implicit none
    
    integer :: status, i, currLineNum, colorNum, total, power
    integer, dimension(3) :: colorVals
    character(len=300) :: line
    character(len=10) :: buffer

    status = 0
    total = 0
    currLineNum = 1

    do while (status == 0)
        read (*, '(a)', iostat=status) line
        if (status /= 0) exit

        buffer = ''
        colorNum = 0
        colorVals = 0

        i = index(line, ": ") + 1
        ! print *, "NEW:", currLineNum, ": ", i, len(trim(line)), trim(line)
        do while (i <= len(trim(line)))
            i = i + 1  ! Moved the incrementing to the top to make sure we process the final character when detecting end of line.
            ! print *, "PROCESSING: ", line(i:i)

            if (line(i:i) == "," .or. line(i:i) == ";" .or. i > len(trim(line))) then
                ! print *, "BUFFER:", buffer
                if (trim(buffer) == 'red') then
                    if (colorVals(1) < colorNum) colorVals(1) = colorNum
                else if (trim(buffer) == 'green') then
                    if (colorVals(2) < colorNum) colorVals(2) = colorNum
                else if (trim(buffer) == 'blue') then
                    if (colorVals(3) < colorNum) colorVals(3) = colorNum
                else
                    print *, "ERROR, WIL ROBINSON. ERROR!"
                end if

                buffer = '';
                colorNum = 0;

                if (i > len(trim(line))) exit;  ! Dropping out after processing last char at end of line.

                i = i + 1  ! Skip the space after delimiter
            else if (line(i:i) == " ") then
                read (buffer, *) colorNum
                buffer = ''
                ! print *, "NUM:", colorNum
            else
                buffer = trim(buffer) // line(i:i)
            end if

            ! print *, "LOOP:", buffer, colorNum, colorVals, trim(line(i:))
        end do

        ! power = colorVals(1) * colorVals(2) * colorVals(3)
        power = product(colorVals)
        total = total + power

        ! print *, "ENDLOOP:", currLineNum, power, colorVals, trim(line)
        currLineNum = currLineNum + 1
    end do

    print *, "PART 2:", total
end program aoc02b