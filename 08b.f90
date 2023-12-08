program aoc08a
    implicit none

    type Map
        character(len=5) :: key, l, r
    endtype
    
    character(len=5), dimension(:), allocatable :: startingAs
    type(Map), dimension(:), allocatable :: maps
    character(len=300) :: line, dirs
    character(len=5) :: currPos
    character :: currDir
    integer :: status, i, k, m
    integer(kind=8) :: total

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

    startingAs = pack(maps%key, maps%key(3:3) == 'A')
    ! print *, "PACKED:", startingAs, size(startingAs)
    ! Run logic
    total = 0
    do k = 1, size(startingAs)
        currPos = startingAs(k)
        i = 1
        do while (currPos(3:3) /= 'Z')
            m = mod(i - 1, len_trim(dirs)) + 1
            currDir = dirs(m:m)
            if (currDir == 'L') then
                currPos = maps(findloc(maps%key, currPos, dim=1))%l
            else
                currPos = maps(findloc(maps%key, currPos, dim=1))%r
            endif
            i = i + 1
        enddo
        ! print *, "CURR I:", i - 1

        if (total == 0) then
            total = int8(i - 1)
        else
            total = lcm(total, int8(i - 1))
        endif
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