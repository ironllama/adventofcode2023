program aoc08a
    implicit none

    type Map
        character(len=5) :: key, l, r
    endtype
    
    type(Map), dimension(:), allocatable :: maps
    character(len=300) :: line, dirs
    character(len=5) :: currPos
    character :: currDir
    integer :: status, i, m

    ! Read the first line and setup
    read (*, *) line
    dirs = trim(line)
    ! print *, "DIRS:"
    
    ! Read the rest of the lines and setup
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit
        if (len_trim(line) == 0) cycle

        if (allocated(maps)) then
            maps = [maps, Map(line(1:3), line(8:10), line(13:15))]
        else
            maps = [Map(line(1:3), line(8:10), line(13:15))]
        endif
    enddo
    ! print *, "IDX:", mapsIdx, "MAPS:", maps

    ! Run logic
    currPos = 'AAA'
    i = 1
    do while (currPos /= 'ZZZ')
        m = mod(i - 1, len_trim(dirs)) + 1
        currDir = dirs(m:m)
        if (currDir == 'L') then
            currPos = maps(findloc(maps%key, currPos, dim=1))%l
        else
            currPos = maps(findloc(maps%key, currPos, dim=1))%r
        endif
        i = i + 1
    enddo

    print *, "PART 1:", i - 1
end program aoc08a