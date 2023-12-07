program aoc05a
    implicit none
    
    character(len=250) :: line
    character(len=20) :: charBuffer
    integer(kind=8) :: status, tempNum, mapLevel, i, dest, source, range
    integer(kind=8), dimension(:), allocatable :: seedNums, nextSeedNums
    ! integer(kind=8), dimension(20) :: seedNums, nextSeedNums

    charBuffer = '';
    tempNum = 0

    read (*, '(a)', iostat=status) line
    do i = 8, len_trim(line)
        if (line(i:i) == ' ' .or. i == len_trim(line)) then
            if (i == len_trim(line)) then
                charBuffer = trim(charBuffer) // line(i:i)
            endif

            if (len_trim(charBuffer) > 0) then
                read (charBuffer, *) tempNum
                if (allocated(seedNums)) then
                    seedNums = [seedNums, tempNum]
                else
                    seedNums = [tempNum]
                endif
            endif

            charBuffer = ''
        else
            charBuffer = trim(charBuffer) // line(i:i)
        endif
    enddo
    ! read (line(8:), *) seedNums  ! Same as above. Much shorter if you know how many elements!
    ! print *, "SEEDS:", seedNums

    nextSeedNums = seedNums
    mapLevel = 0
    charBuffer = ''
    do while (.not. is_iostat_end(status))
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        if (len_trim(line) == 0) cycle

        if (index(line, ":") /= 0) then
            mapLevel = mapLevel + 1
            ! print *, "NEW LEVEL:", mapLevel

            seedNums = nextSeedNums
        else
            read (line, *) dest, source, range
            do i = 1, size(seedNums)
                ! print *, "LEVEL:", mapLevel, "MAP:", mapNums
                if (seedNums(i) /= nextSeedNums(i)) cycle
                if (seedNums(i) >= source .and. seedNums(i) < (source + range)) then
                    nextSeedNums(i) = seedNums(i) - (source - dest)
                endif
            enddo
            ! print *, "LEVEL:", mapLevel, "MAP:", mapNums, "SEED:", seedNums, "NEXT:", nextSeedNums

        endif
    enddo
    
    ! print *, "END SEEDS:", nextSeedNums
    print *, "PART 1:", minVal(nextSeedNums)
end program aoc05a