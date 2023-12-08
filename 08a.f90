program aoc08a
    implicit none

    type Map
        character(len=5) :: l, r
    endtype
    
    character, dimension(:), allocatable :: dirs
    character(len=5), dimension(:), allocatable :: mapsIdx
    type(Map), dimension(:), allocatable :: maps
    character(len=300) :: line
    character(len=5) :: currPos
    character :: currDir
    integer :: status, i

    ! Read the first line and setup
    read (*, *) line
    ! read (line, *) dirs(:)  ! WHY NOT?!
    do i = 1, len_trim(line)
        if (allocated(dirs)) then
            dirs = [dirs, line(i:i)]
        else
            dirs = [line(i:i)]
        endif
    enddo
    ! print *, "DIRS:"
    ! do i = 1, size(dirs)
    !     print *, dirs(i)
    ! enddo
    
    ! Read the rest of the lines and setup
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit
        if (len_trim(line) == 0) cycle

        if (allocated(mapsIdx)) then
            mapsIdx = [mapsIdx, line(1:3)]
        else
            mapsIdx = [line(1:3)]
        endif

        if (allocated(maps)) then
            maps = [maps, Map(line(8:10), line(13:15))]
        else
            maps = [Map(line(8:10), line(13:15))]
        endif
    enddo
    ! print *, "IDX:", mapsIdx, "MAPS:", maps

    ! Run logic
    currPos = 'AAA'
    i = 1
    do while (currPos /= 'ZZZ')
        currDir = dirs(mod(i - 1, size(dirs)) + 1)
        if (currDir == 'L') then
            currPos = maps(findloc(mapsIdx, currPos, dim=1))%l
        else
            currPos = maps(findloc(mapsIdx, currPos, dim=1))%r
        endif
        i = i + 1
    enddo

    print *, "PART 1:", i - 1
end program aoc08a