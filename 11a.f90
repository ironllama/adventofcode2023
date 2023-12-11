program aoc11a
    implicit none
    
    type Coords
        integer :: x, y
    endtype

    character(len=200) :: line
    character(len=:), allocatable :: allLines(:)
    character, allocatable :: allChars(:,:)
    integer :: status, i, k, total
    type(Coords), allocatable :: allPairs(:)

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

    call expandSpace(allChars)
    ! print *, "EXPAND:", size(allChars, 1), size(allChars, 2)
    ! do i = 1, size(allChars, 1)
    !     ! print *, (allChars(i, k), k = 1, size(allChars, 2))
    !     print *, allChars(i,:)
    ! enddo

    call rotateArrayCounter(allChars)
    ! print *, "ROT:", size(allChars, 1), size(allChars, 2)
    ! do i = 1, size(allChars, 1)
    !     print *, allChars(i,:)
    ! enddo

    call expandSpace(allChars)
    ! print *, "EXPAND:", size(allChars, 1), size(allChars, 2)
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

    total = 0
    do i = 1, size(allPairs)
        do k = i + 1, size(allPairs)
            total = total + abs(allPairs(i)%x - allPairs(k)%x) + abs(allPairs(i)%y - allPairs(k)%y)
        enddo
    enddo
    print *, "PART 1:", total

contains
    subroutine expandSpace(inArray)
        implicit none
        character, allocatable, intent(inout) :: inArray(:,:)
        character, allocatable :: newArray(:,:)
        character, allocatable :: tempArray(:,:)
        integer :: rows, cols, i, posNew

        rows = size(inArray, 1)
        cols = size(inArray, 2)
        allocate(newArray(rows, cols))

        posNew = 1
        do i = 1, size(inArray, 1)
            newArray(posNew,:) = inArray(i,:)
            posNew = posNew + 1

            if (all(inArray(i,:) == '.')) then
                rows = rows + 1
                allocate(tempArray(rows, cols))
                ! tempArray(1:posNew-1,:) = newArray(:,:)
                tempArray(:,:) = newArray(:,:)  ! Same as above?!
                call move_alloc(tempArray, newArray)  ! Automatic deallocate of tempArray

                newArray(posNew,:) = inArray(i,:)
                posNew = posNew + 1
            endif
        enddo
        inArray = newArray

        deallocate(newArray)
    endsubroutine

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
end program aoc11a