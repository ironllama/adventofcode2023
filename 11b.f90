program aoc11b
    implicit none
    
    type Coords
        integer :: y, x
    endtype

    character(len=200) :: line
    character(len=:), allocatable :: allLines(:)
    character, allocatable :: allChars(:,:)
    integer :: status, i, k, m
    integer(kind=8) :: total, shortX, shortY
    type(Coords), allocatable :: allPairs(:)
    integer, allocatable :: blankRows(:), blankCols(:)

    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        if (allocated(allLines)) then
            allLines = [allLines, line]
        else
            allLines = [line]
        endif
    enddo
    ! print *, "LINES:"
    ! do i = 1, size(allLines)
    !     print *, trim(allLines(i))
    ! enddo

    ! Turn string lines into character array lines.
    allocate(allChars(size(allLines), len_trim(allLines(1))))
    do i = 1, size(allLines)
        do k = 1, len_trim(allLines(i))
            allChars(i,k) = allLines(i)(k:k)
        enddo
    enddo

    ! Find blank rows.
    do i = 1, size(allChars, 1)  ! Don't forget the dim value! Otherwise numbers go wonky.
        if (all(allChars(i,:) == '.')) then
            if (allocated(blankRows)) then
                blankRows = [blankRows, i]
            else
                blankRows = [i]
            endif
        endif
    enddo

    call rotateArrayCounter(allChars)
    ! print *, "ROT:", size(allChars, 1), size(allChars, 2)
    ! do i = 1, size(allChars, 1)
    !     print *, allChars(i,:)
    ! enddo

    ! Find blank cols.
    do i = 1, size(allChars, 1)
        if (all(allChars(i,:) == '.')) then
            if (allocated(blankCols)) then
                blankCols = [blankCols, i]
            else
                blankCols = [i]
            endif
        endif
    enddo

    ! Rotate back upright.
    call rotateArrayCounter(allChars)
    call rotateArrayCounter(allChars)
    call rotateArrayCounter(allChars)
    ! print *, "BACK:", size(allChars, 1), size(allChars, 2)
    ! do i = 1, size(allChars, 1)
    !     print *, allChars(i,:)
    ! enddo

    do i = 1, size(allChars, 1)
        do k = 1, size(allChars, 2)
            if (allChars(i, k) == '#') then
                if (allocated(allPairs)) then
                    allPairs = [allPairs, Coords(i, k)]
                else
                    allPairs = [Coords(i, k)]
                endif
            endif
        enddo
    enddo
    ! print *, "ALL PAIRS:", size(allPairs)
    ! do i = 1, size(allPairs)
    !     print *, "[", allPairs(i), "]"
    ! enddo

    ! print *, "CHECK:", size(blankRows), size(blankCols)
    ! print *, "CHECK:", "ROWS:", blankRows, "COLS:", blankCols
    total = 0
    do i = 1, size(allPairs) - 1
        do k = i + 1, size(allPairs)
            shortY = int8(abs(allPairs(i)%y - allPairs(k)%y))
            do m = 1, size(blankRows)
                if ((blankRows(m) < allPairs(i)%y .and. blankRows(m) > allPairs(k)%y) .or. &
                        (blankRows(m) > allPairs(i)%y .and. blankRows(m) < allPairs(k)%y)) shortY = shortY + 999999
            enddo

            shortX = int8(abs(allPairs(i)%x - allPairs(k)%x))
            do m = 1, size(blankCols)
                if ((blankCols(m) < allPairs(i)%x .and. blankCols(m) > allPairs(k)%x) .or. &
                        (blankCols(m) > allPairs(i)%x .and. blankCols(m) < allPairs(k)%x)) shortX = shortX + 999999
            enddo

            ! print *, "COMBO:", i, k, allPairs(i)%y, allPairs(k)%y, shortY, allPairs(i)%x, allPairs(k)%x, shortX
            total = total + shortY + shortX
        enddo
    enddo

    print *, "PART 2:", total

contains
    subroutine rotateArrayCounter(inArray)
        implicit none
        character, allocatable, intent(inout) :: inArray(:,:)
        character, allocatable :: tempArray(:,:)
        integer :: rows, cols, i, j
      
        rows = size(inArray, 1)
        cols = size(inArray, 2)
        allocate(tempArray(cols, rows))
      
        do i = 1, rows
           do j = 1, cols
              tempArray(j, rows - i + 1) = inArray(i, j)
           end do
        end do
        ! inArray = tempArray  ! Different shapes. Messing things up.
        call move_alloc(tempArray, inArray)
      
    endsubroutine
end program aoc11b