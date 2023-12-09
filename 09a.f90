program aoc09a
    implicit none

    type Level
        integer, allocatable :: vals(:)
    endtype

    character(len=200) :: line
    integer :: numNums, i, status, newVal, total
    type(Level) :: tempLevel
    type(Level), dimension(:), allocatable :: lineVals
    logical :: keepGoing

    total = 0

    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        tempLevel = Level()
        numNums = count([(line(i:i) == ' ', i = 1, len_trim(line))]) + 1
        allocate(tempLevel%vals(numNums))
        read (line, *) tempLevel%vals

        if (allocated(lineVals)) then
            lineVals = [lineVals, tempLevel]
        else
            lineVals = [tempLevel]
        endif
        ! print *, "LINEVALS:", lineVals

        i = 1
        do
            tempLevel = Level()
            allocate(tempLevel%vals(numNums - i))
            tempLevel%vals = lineVals(i)%vals(2:) - lineVals(i)%vals(1:size(lineVals(i)%vals)-1)
            ! print *, "TEMPLEVEL:", tempLevel%vals
            lineVals = [lineVals, tempLevel]

            if (count(tempLevel%vals /= 0) == 0) exit
            i = i + 1
        enddo

        newVal = 0
        i = size(lineVals) - 1
        do while (i > 0)
            newVal = lineVals(i)%vals(size(lineVals(i)%vals)) + newVal
            lineVals(i)%vals = [lineVals(i)%vals, newVal]
            ! print *, "NEW LINEVALS:", lineVals(i-1)%vals

            i = i - 1
        enddo
        total = total + newVal

        deallocate(lineVals)
    enddo

    print *, "TOTAL: ", total

end program aoc09a