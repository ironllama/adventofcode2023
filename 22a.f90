program aoc22a
    implicit none
    
    type Block
        integer :: id, from(3), to(3)
    endtype

    character(len=20) :: line
    integer :: status, i, pos, from(3), to(3), temp(3), total, k
    type(Block), allocatable :: allBlocks(:), compacted(:), tempAllBlocks(:), origCompacted(:), tempCompacted(:)
    type(Block) :: curr
    character(len=17000) :: cKey, tempKey
    real :: start, finish

    i = 1
    allocate(allBlocks(0))
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        pos = index(line, "~")
        read (line(1:pos-1), *) from
        read (line(pos+1:), *) to
        if (from(3) > to(3)) then
            temp = from
            from = to
            to = temp
        endif
        curr = Block(i, from, to)
        allBlocks = [allBlocks, curr] 
        i = i + 1
    end do
    ! call quicksort(allBlocks, 1, size(allBlocks), 3)
    call insertionsort(allBlocks, 3)
    ! write (cKey, '(*(i0i0i0i0i0i0i0,:,"-"))') allBlocks
    ! print *, "S: ", trim(cKey)

    origCompacted = compact(allBlocks)
    write (cKey, '(*(i0i0i0i0i0i0i0,:,"-"))') origCompacted
    ! print *, "0: ", trim(cKey)
    ! print *, "0: ", len_trim(cKey)

    total = 0
    call cpu_time(start)
    call cpu_time(finish)
    do i = 1, size(allBlocks)
        ! print *, i, ": ", finish-start, "ETA (min): ", (((finish-start) / 60) * (size(allBlocks) - i - 1))
        call cpu_time(start)

        tempCompacted = origCompacted
        pos = 0
        do k = 1, size(tempCompacted)
            if (tempCompacted(k)%id == allBlocks(i)%id) then
                tempCompacted = [tempCompacted(1:k-1), tempCompacted(k+1:)]
                exit
            endif
        enddo
        write (cKey, '(*(i0i0i0i0i0i0i0,:,"-"))') tempCompacted
        ! print *, "1: ", trim(cKey)
        ! print *, size(tempCompacted), len_trim(cKey)

        tempAllBlocks = allBlocks
        do k = 1, size(tempAllBlocks)
            if (tempAllBlocks(k)%id == allBlocks(i)%id) then
                tempAllBlocks = [tempAllBlocks(1:k-1), tempAllBlocks(k+1:)]
                exit
            endif
        enddo

        compacted = compact(tempAllBlocks)
        write (tempKey, '(*(i0i0i0i0i0i0i0,:,"-"))') compacted
        ! print *, "2: ", trim(tempKey)
        ! print *, size(compacted), len_trim(tempKey)

        if (len_trim(cKey) == len_trim(tempKey) .and. trim(cKey) == trim(tempKey)) then
            ! print *, "SAME: ", i
            total = total + 1
        endif

        call cpu_time(finish)
    enddo

    print *, "PART 1:", total

contains
    integer function unitLevel(tKey, z, tower)
        character(len=*), intent(in) :: tKey
        integer, intent(in) :: z
        character(len=*), allocatable, intent(inout) :: tower(:)
        integer :: i

        do i = z, 1, -1
            if (index(trim(tower(i)), trim(tKey)) > 0) then  ! trim(tower) to speed things up!
                ! print *, "HIGHEST: ", i + 1
                unitLevel = i + 1
                return
            endif
        enddo
        unitLevel = 1
    endfunction

    function compact(before) result(after)
        type(Block), intent(in) :: before(:)
        type(Block), allocatable :: after(:)
        integer :: i, k, dim, low, high, highest, y, x, z, zTop
        logical :: longblock
        character(len=350), allocatable :: tower(:)  ! Tuned to the input data.
        character(len=10) :: tKey

        ! Get size of after and preallocate tower and after for better performance.
        after = before

        highest = 0
        do i = 1, size(after)
            if (after(i)%to(3) > highest) highest = after(i)%to(3)
        enddo
        allocate(tower(highest))
        tower = ""

        do i = 1, size(after)
            y = after(i)%from(1)
            x = after(i)%from(2)
            z = after(i)%from(3)
            ! print *, "after(i):", after(i)%id

            longblock = .false.
            do dim = 1, 3
                if (after(i)%from(dim) /= after(i)%to(dim)) then

                    low = min(after(i)%from(dim), after(i)%to(dim))
                    high = max(after(i)%from(dim), after(i)%to(dim))
                    if (dim < 3) then
                        highest = 0

                        ! Find the highest level for each unit of block to find where it stops.
                        do k = low, high
                            if (dim == 1) then
                                write(tKey, '("[",i0,",",i0,"]")') k, x
                            else
                                write(tKey, '("[",i0,",",i0,"]")') y, k
                            endif
                            highest = max(highest, unitLevel(tKey, z, tower))
                        enddo

                        ! Update tower info
                        do k = low, high
                            if (dim == 1) then
                                write(tKey, '("[",i0,",",i0,"]")') k, x
                            else
                                write(tKey, '("[",i0,",",i0,"]")') y, k
                            endif
                            tower(highest) = trim(tower(highest)) // tKey
                        enddo

                        after(i)%from(3) = highest
                        after(i)%to(3) = highest
                    else  ! Handle the tall blocks that span rows
                        write(tKey, '("[",i0,",",i0,"]")') y, x
                        highest = unitLevel(tKey, z, tower)
                        zTop = highest + (after(i)%to(3) - after(i)%from(3))

                        do k = highest, zTop
                            tower(k) = trim(tower(k)) // tKey
                        enddo

                        after(i)%from(3) = highest
                        after(i)%to(3) = zTop
                    endif

                    longblock = .true.
                    exit
                endif
            enddo

            if (.not. longblock) then  ! Handle the 1x1x1 odd blocks
                write(tKey, '("[",i0,",",i0,"]")') y, x
                highest = unitLevel(tKey, z, tower)

                tower(highest) = trim(tower(highest)) // tKey

                after(i)%from(3) = highest
                after(i)%to(3) = highest
            endif
        enddo
        ! after = after(1:aLen-1)
    end function

    recursive subroutine quicksort(arr, beg, end, axis)
        type(Block), intent(inout) :: arr(:)
        integer, intent(in) :: beg, end, axis
        integer :: i, k
        type(Block) :: pivot, temp

        if (beg < end) then
            pivot = arr(end)
            i = beg - 1

            do k = beg, end - 1
                if (arr(k)%from(axis) <= pivot%from(axis)) then  ! Sort by this field/property
                i = i + 1
                temp = arr(i)
                arr(i) = arr(k)
                arr(k) = temp
                end if
            end do

            temp = arr(i + 1)
            arr(i + 1) = arr(end)
            arr(end) = temp

            ! Recursively sort the sub-arrays
            call quicksort(arr, beg, i, axis)
            call quicksort(arr, i + 2, end, axis)
        end if
    end subroutine quicksort

    recursive subroutine insertionsort(array, axis)
        type(Block), intent(inout) :: array(:)
        integer, intent(in) :: axis
        type(Block) :: temp
        integer :: i, j, last

        last=size(array)
        do i = 2, last
            temp = array(i)
            do j = i-1, 1, -1
                if (array(j)%from(axis) .le. temp%from(axis)) exit
                array(j+1) = array(j)
            enddo
            array(j+1) = temp
        enddo
        return
    end subroutine insertionsort
end program aoc22a