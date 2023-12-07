program aoc05b
    implicit none
    
    character(len=250) :: line
    character(len=20) :: charBuffer
    integer(kind=8) :: status, tempNum, mapLevel, i, &
                        k, dest, source, range, tempNumLast, overlapBeg, overlapEnd
    integer(kind=8), dimension(:), allocatable :: seedNums, nextSeedNums
    integer(kind=8), dimension(:), allocatable :: mapNums
    integer(kind=8), dimension(2) :: newRange
    logical :: rangeFound

    charBuffer = '';
    tempNum = 0
    tempNumLast = 0

    read (*, '(a)', iostat=status) line
    do i = 8, len_trim(line)
        if (line(i:i) == ' ' .or. i == len_trim(line)) then
            if (i == len_trim(line)) then
                charBuffer = trim(charBuffer) // line(i:i)
            endif

            if (len_trim(charBuffer) > 0) then
                read (charBuffer, *) tempNum

                if (tempNumLast /= 0) then
                    if (allocated(seedNums)) then
                        seedNums = [seedNums, tempNumLast, tempNumLast + (tempNum - 1)]
                    else
                        seedNums = [tempNumLast, tempNumLast + (tempNum - 1)]
                    endif
                    tempNumLast = 0
                else
                    tempNumLast = tempNum
                endif
            endif

            charBuffer = ''
        else
            charBuffer = trim(charBuffer) // line(i:i)
        endif
    enddo
    ! print *, "SEEDS:", seedNums

    ! nextSeedNums = seedNums
    mapLevel = 0
    charBuffer = ''
    newRange = 0
    do while (.not. is_iostat_end(status))  ! Loop through all the map blocks
        do  ! Loop through each map in a mapblock
            read (*, '(a)', iostat=status) line
            ! print *, "READ:", line
            if (is_iostat_end(status)) exit
            if (len_trim(line) == 0) exit
            if (index(line, ":") /= 0) cycle

            read (line, *) dest, source, range
            if (allocated(mapNums)) then
                mapNums = [mapNums, dest, source, range]
            else
                mapNums = [dest, source, range]
            endif
        enddo
        ! print *, "LEVEL:", mapLevel, "MAP:", mapNums

        ! print *, "LEVEL:", mapLevel, "SEED:", seedNums, "NEXT:", nextSeedNums
        i = 1
        do while (allocated(mapNums) .and. i < size(seedNums))  ! Loop for each seed range that needs checking
            rangeFound = .false.
            k = 1
            do while (k < size(mapNums))  ! Loop over all the found mapNums
                dest = mapNums(k)
                source = mapNums(k + 1)
                range = mapNums(k + 2)

                overlapBeg = max(seedNums(i), source)
                overlapEnd = min(seedNums(i + 1), source + range)

                ! print *, "TEST: SEEDNUM:", seedNums(i), seedNums(i + 1), "MAP:", dest, source, range
                ! print *, "OVER:", overlapBeg, overlapEnd

                if (overlapBeg < overlapEnd) then  ! Overlap/intersection!
                    ! print *, "LEVEL:", mapLevel, "MAP:", mapNums
                    newRange = [overlapBeg - source + dest, overlapEnd - source + dest]
                    if (allocated(nextSeedNums)) then
                        nextSeedNums = [nextSeedNums, newRange]
                    else
                        nextSeedNums = [newRange]
                    endif

                    ! Reinsert "ends" for further processing by other maps.
                    if (seedNums(i) < overlapBeg) seedNums = [seedNums, seedNums(i), overlapBeg]
                    if (seedNums(i + 1) > overlapEnd) seedNums = [seedNums, overlapEnd, seedNums(i + 1)]

                    rangeFound = .true.
                    exit
                endif

                k = k + 3
            enddo

            if (.not. rangeFound) then
                if (allocated(nextSeedNums)) then
                    nextSeedNums = [nextSeedNums, seedNums(i), seedNums(i + 1)]
                else
                    nextSeedNums = [seedNums(i), seedNums(i + 1)]
                endif
            endif
            ! print *, "NEXT:", nextSeedNums

            i = i + 2
        enddo
        ! print *, "LEVEL:", mapLevel, "MAP:", mapNums, "SEED:", seedNums, "NEXT:", nextSeedNums
        
        ! Reset for next map block
        if (allocated(nextSeedNums)) then
            seedNums = nextSeedNums
            deallocate(nextSeedNums);
        endif
        if (allocated(mapNums)) deallocate(mapNums)

        mapLevel = mapLevel + 1
    enddo
    
    print *, "PART 2:", minVal(seedNums)
end program aoc05b