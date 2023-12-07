program aoc06a
    implicit none
    
    character(len=50) :: values
    character(len=10) :: key
    ! integer, dimension(3) :: times, distances
    integer, dimension(4) :: times, distances
    integer :: total, breakers, speed, i, newDistance

    read (*, '(a10, a)') key, values
    read (values, *) times
    read (*, '(a10, a)') key, values
    read (values, *) distances
    ! print *, "TIMES:", times, "DIST:", distances

    total = 1

    do i = 1, size(times)
        breakers = 0

        do speed = 1, times(i) - 1
            newDistance = speed * (times(i) - speed)
            if (newDistance > distances(i)) breakers = breakers + 1
        enddo

        total = total * breakers
    enddo

    print *, "PART 1:", total
end program aoc06a