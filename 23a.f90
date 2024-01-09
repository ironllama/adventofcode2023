program aoc23a
    implicit none

    type Coord
        integer y, x
    endtype

    type qItem
        type(Coord) :: val
        type(Coord), allocatable :: path(:)
    endtype qItem

    character(len=200), allocatable :: lines(:)
    character(len=200) :: line
    integer :: status, i, k, newDist, total=1
    integer, allocatable :: dist(:,:)
    type(qItem), target, allocatable :: pq(:)
    type(qItem) :: curr, next
    type(Coord) :: beg, end, dirs(4)
    type(Coord), allocatable :: newDirs(:), endPath(:)

    allocate(lines(0))
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit
        lines = [lines, line]
    end do
    
    ! Close out the endings to avoid bounds checking while pathfinding
    beg = Coord(2, 2)
    end = Coord(size(lines) - 1, len_trim(lines(1))-1)
    lines(1)(2:2) = "#"
    lines(size(lines))(len_trim(lines(1))-1:len_trim(lines(1))-1) = "#"
    ! print *, "LINES:"
    ! do i = 1, size(lines)
    !     print *, trim(lines(i))
    ! enddo

    allocate(dist(size(lines), len_trim(lines(1))))
    dist = 0

    pq = [qItem(beg)]
    allocate(pq(1)%path(0))

    dirs = [Coord(1,0), Coord(0,1), Coord(-1,0), Coord(0,-1)]

    do while (size(pq) > 0)
        curr = pq(size(pq))  ! Take from back of stack.
        pq = pq(:size(pq)-1)
        ! curr = pq(1)  ! Take from front of queue.
        ! pq = pq(2:)
        ! print *, "SIZE:", size(pq), " CURR:", curr%val, " PATH:", size(curr%path)

        ! Record any endings...
        if (curr%val%y == end%y .and. curr%val%x == end%x) then
            if (size(curr%path) > total) then  ! The longest ending.
                endPath = curr%path
                total = size(endPath)
            endif
        endif

        ! Directed paths.
        if (lines(curr%val%y)(curr%val%x:curr%val%x) == ">") then
            newDirs = [Coord(0,1)]
        else if (lines(curr%val%y)(curr%val%x:curr%val%x) == "v") then
            newDirs = [Coord(1,0)]
        else
            newDirs = dirs
        endif

        ! All neighbors.
        dirsLoop: do i=1, size(newDirs)
            next = curr
            next%val%y = next%val%y + newDirs(i)%y
            next%val%x = next%val%x + newDirs(i)%x
            ! print *, "NEXT:", next%val, newDirs(i)

            if (lines(next%val%y)(next%val%x:next%val%x) /= '#') then
                do k=1, size(curr%path)
                    if (curr%path(k)%y == next%val%y .and. curr%path(k)%x == next%val%x) cycle dirsLoop ! Need to make sure not to go in circles!
                enddo

                newDist = dist(curr%val%y, curr%val%x) + 1
                if (newDist > dist(next%val%y, next%val%x)) then
                    dist(next%val%y, next%val%x) = newDist
                    next%path = [next%path, curr%val]
                    pq = [pq, next]
                endif
            endif
        enddo dirsLoop
    enddo
    ! print *, "PATH:"
    ! do i=1, size(lines)
    !     line = ""
    !     do k=1, len_trim(lines(i))
    !         if (any(endPath(:)%y == i .and. endPath(:)%x == k)) then
    !             line = trim(line) // "O"
    !         else
    !             line = trim(line) // lines(i)(k:k)
    !         endif
    !     enddo
    !     print *, line
    ! enddo
    ! print *, "LAST DIST:", dist(size(dist,1)+1, size(dist,2)-1)
    ! print *, "DIST:"
    ! do i = 1, size(dist, 1)
    !     print *, dist(i,:)
    ! enddo

    print *, "PART 1:", total + 2  ! Replace the two steps removed.

    ! command took 4:6:20.02 (14780.02s total)  ! Standard BFS with array and pos tracking, array keeps growing
    ! command took 0:0:21.39 (21.39s total)  ! BFS with queue, array shrinks
    ! command took 0:0:0.40 (0.40s total) ! DFS with stack, array shrinks

    deallocate(dist)
end program aoc23a