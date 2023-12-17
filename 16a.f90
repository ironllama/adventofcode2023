program aoc16a
    implicit none

    type QueueItem
        integer :: yx(2), dir(2)
    endtype

    character(len=150), allocatable :: lines(:), trackedLines(:)
    character(len=150) :: line
    integer :: status, total = 0, qPos = 1
    type(QueueItem), allocatable :: queue(:)
    type(QueueItem) :: curr
    character(len=20) :: qKey
    character(len=20), allocatable :: allHistory(:)
    character :: tile

    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit
        
        if (allocated(lines)) then
            lines = [lines, line]
        else
            lines = [line]
        endif
    enddo
    ! print *, "LINES:"
    ! do i = 1, size(lines)
    !     print *, trim(lines(i))
    ! enddo

    trackedLines = lines;

    queue = [QueueItem([1, 1], [0, 1])]
    do while(qPos <= size(queue))
        curr = queue(qPos)
        ! print *, "START", curr

        write(qKey, '(i0,a,i0,a,i0,i0)') curr%yx(1), ",", curr%yx(2), " ", curr%dir
        ! print *, "KEY:[", trim(qKey), "]"
        if (allocated(allHistory)) then
            if (any(allHistory == qKey)) then
                qPos = qPos + 1
                cycle
            endif
            allHistory = [allHistory, qKey]
        else
            allHistory = [qKey]
        endif

        if (trackedLines(curr%yx(1))(curr%yx(2):curr%yx(2)) /= '#') then
            trackedLines(curr%yx(1))(curr%yx(2):curr%yx(2)) = '#'
            total = total + 1
        endif

        tile = lines(curr%yx(1))(curr%yx(2):curr%yx(2))
        ! print *, "TILE:", tile
        select case (tile)
        case ('/')
            if (curr%dir(1) < 0) then
                call addQueue(curr%yx, [0, 1])
            else if (curr%dir(1) > 0) then
                call addQueue(curr%yx, [0, -1])
            else if (curr%dir(2) < 0) then
                call addQueue(curr%yx, [1, 0])
            else if (curr%dir(2) > 0) then
                call addQueue(curr%yx, [-1, 0])
            endif
        case ('\')
            if (curr%dir(1) < 0) then
                call addQueue(curr%yx, [0, -1])
            else if (curr%dir(1) > 0) then
                call addQueue(curr%yx, [0, 1])
            else if (curr%dir(2) < 0) then
                call addQueue(curr%yx, [-1, 0])
            else if (curr%dir(2) > 0) then
                call addQueue(curr%yx, [1, 0])
            endif
        case ('-')
            if (curr%dir(1) /= 0) then
                call addQueue(curr%yx, [0, 1])
                call addQueue(curr%yx, [0, -1])
            else
                call addQueue(curr%yx, curr%dir)
            endif
        case ('|')
            if (curr%dir(2) /= 0) then
                call addQueue(curr%yx, [1, 0])
                call addQueue(curr%yx, [-1, 0])
            else
                call addQueue(curr%yx, curr%dir)
            endif
        case default
            call addQueue(curr%yx, curr%dir)
        endselect

        qPos = qPos + 1
        ! print *, "QUEUE:", qPos, size(queue)
    enddo

    ! print *, "TRACKED:"
    ! do i = 1, size(trackedLines)
    !     print *, trim(trackedLines(i))
    ! enddo

    print *, "PART 1:", total

contains
    subroutine addQueue(newYX, newDir)
        integer, intent(in) :: newYX(:), newDir(:)
        integer :: subYX(2)

        subYX(1) = newYX(1) + newDir(1)
        subYX(2) = newYX(2) + newDir(2)
        if (subYX(1) > 0 .and. subYX(1) <= size(lines) .and. subYX(2) > 0 .and. subYX(2) <= len_trim(lines(1))) then
            ! print *, "NEW:", subYX, newDir
            queue = [queue, QueueItem(subYX, newDir)]
        endif
    endsubroutine
end program aoc16a