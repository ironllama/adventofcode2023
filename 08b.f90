program aoc08a
    implicit none

    type Map
        character(len=5) :: l, r
    endtype
    
    character, dimension(:), allocatable :: dirs
    character(len=5), dimension(:), allocatable :: mapsIdx, startingAs
    type(Map), dimension(:), allocatable :: maps
    character(len=300) :: line
    character(len=5) :: currPos
    character :: currDir
    integer, dimension(:), allocatable :: minSteps
    integer :: status, i, k
    integer(kind=8) :: total

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

        if (line(3:3) == 'A') then
            if (allocated(startingAs)) then
                startingAs = [startingAs, line(1:3)]
            else
                startingAs = [line(1:3)]
            endif
        endif
    enddo
    ! print *, "IDX:", mapsIdx, "MAPS:", maps

    ! Run logic
    do k = 1, size(startingAs)
        currPos = startingAs(k)
        i = 1
        do while (currPos(3:3) /= 'Z')
            currDir = dirs(mod(i - 1, size(dirs)) + 1)
            if (currDir == 'L') then
                currPos = maps(findloc(mapsIdx, currPos, dim=1))%l
            else
                currPos = maps(findloc(mapsIdx, currPos, dim=1))%r
            endif
            i = i + 1
        enddo

        if (allocated(minSteps)) then
            minSteps = [minSteps, i - 1]
        else
            minSteps = [i - 1]
        endif
    enddo
    ! print *, "MIN:", minSteps

    total = int8(minSteps(1))
    do k = 2, size(minSteps)
        total = lcm(total, int8(minSteps(k)))
        ! print *, "TOTAL:", total  ! Evidence of overflow! Upgrade to kind=8
    enddo
    print *, "PART 2:", total

contains
    ! Adapted from: https://stackoverflow.com/questions/47047682/least-common-multiple-of-an-array-values-using-euclidean-algorithm
    recursive function gcd (a, b) result (out)
        integer(kind=8), intent(in) :: a, b
        integer(kind=8) :: out
        if (a > 0) then
            out = gcd(mod(b, a), a)
        else
            out = b
        endif
    endfunction

    recursive function lcm (a, b) result (out)
        integer(kind=8), intent(in) :: a, b
        integer(kind=8) :: out
        out = (a * b) / gcd(a, b)
    endfunction
end program aoc08a