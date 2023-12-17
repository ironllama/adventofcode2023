program aoc17a
    use PriorityQueue
    implicit none

    type Coords
        integer :: y, x
    endtype

    integer, allocatable :: lines(:,:), tempLines(:,:)
    character(len=200) :: line
    integer :: status, lineNum = 1, i, k, shortest = 99999
    type(Coords) :: dirs(4), target
    type(t_priority_queue) :: pq
    type(t_queue_item) :: curr, next
    character(len=20), allocatable :: allHistory(:)
    character(len=20) :: hKey

    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        if (allocated(lines)) then
            allocate(tempLines(lineNum, len_trim(line)))
            tempLines(1:lineNum - 1,:) = lines
            call move_alloc(tempLines, lines)
        else
            allocate(lines(lineNum, len_trim(line)))
        endif

        do i = 1, len_trim(line)
            read(line(i:i), *) lines(lineNum,i:i)
        enddo
        ! print *, "LINE[", lineNum, "]: ", lines(lineNum,1:)

        lineNum = lineNum + 1
    enddo

    dirs = [Coords(1, 0), Coords(0, 1), Coords(-1, 0), Coords(0, -1)]
    target = Coords(size(lines, 1), size(lines, 2))

    call insert(pq, t_queue_item(0, [1, 1, 1, 1]))

    do while(pq%num > 0)
        curr = pull(pq)
        ! print *, "CURR:", curr%p, curr%val

        write (hKey, '(*(i0,:,","))') curr%val
        ! print *, "HKEY: ", trim(hKey)
        if (allocated(allHistory)) then
            if (any(allHistory == hKey)) then
                cycle
            endif
            allHistory = [allHistory, hKey]
        else
            allHistory = [hKey]
        endif

        if (curr%val(1) == target%y .and. curr%val(2) == target%x) then
            ! print *, "ENDING:", curr%p
            if (curr%p < shortest) then
                shortest = curr%p
                exit
            endif
        endif

        do i = 1, size(dirs)
            if ((dirs(i)%y == curr%val(3) .and. dirs(i)%x == curr%val(4)) &
                .or. (dirs(i)%y == (-1 * curr%val(3)) .and. dirs(i)%x == (-1 * curr%val(4)))) cycle

            next = curr;
            next%val(3) = dirs(i)%y
            next%val(4) = dirs(i)%x

            do k = 1, 3
                next%val(1) = next%val(1) + dirs(i)%y
                next%val(2) = next%val(2) + dirs(i)%x
                ! print *, "NEXT:", next%val
                if (next%val(1) > 0 .and. next%val(1) <= size(lines, 1) &
                    .and. next%val(2) > 0 .and. next%val(2) <= size(lines, 2)) then
                    next%p = next%p + lines(next%val(1), next%val(2))
                    call insert(pq, next)
                endif
            enddo
        enddo
    enddo

    print *, "PART 1:", shortest  ! Takes about 3.5 minutes!

end program aoc17a