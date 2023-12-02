program aoc02a
    implicit none
    
    integer :: status, i, currLineNum, colorNum, total
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
        colorVals = [12, 13, 14]

        i = index(line, ": ") + 1
        ! print *, "NEW:", currLineNum, ": ", i, len(trim(line)), trim(line)
        do while (i <= len(trim(line)))
            i = i + 1
            ! print *, "PROCESSING: ", line(i:i)

            if (line(i:i) == "," .or. line(i:i) == ";" .or. i > len(trim(line))) then
                ! print *, "BUFFER:", buffer
                if (trim(buffer) == 'red') then
                    colorVals(1) = colorVals(1) - colorNum
                    if (colorVals(1) < 0) exit;
                else if (trim(buffer) == 'green') then
                    colorVals(2) = colorVals(2) - colorNum
                    if (colorVals(2) < 0) exit;
                else if (trim(buffer) == 'blue') then
                    colorVals(3) = colorVals(3) - colorNum
                    if (colorVals(3) < 0) exit;
                else
                    print *, "ERROR, WIL ROBINSON. ERROR!"
                end if

                buffer = '';
                colorNum = 0;

                if (i > len(line)) exit;

                if (line(i:i) == ";") then
                    colorVals = [12, 13, 14]
                end if
                
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

        ! print *, "ENDLOOP:", currLineNum, colorVals, trim(line)
        if (minval(colorVals) >= 0) then  ! If all values are positive, or valid
            total = total + currLineNum
            ! print *, "GOOD LINE:", currLineNum, colorVals, trim(line)
        end if

        currLineNum = currLineNum + 1
    end do

    print *, "PART 1:", total
end program aoc02a