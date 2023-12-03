program aoc03a
    implicit none

    type Part
        integer :: row, col, val
    end type

    interface operator(==)
        procedure part_equal
    end interface

    character(len=150), dimension(:), allocatable :: lines
    character(len=150) :: buffer
    character(len=10), parameter :: numStrs = "0123456789"
    integer :: status, row, col, pos, i, total
    type(Part), dimension(:), allocatable :: matches

    total = 0
    ! matches = [Part(0, 0, 0)]

    do while (.not. is_iostat_end(status))
        read(*, '(a)', iostat=status) buffer
        if (allocated(lines)) then
            lines = [lines, buffer]
        else
            lines = [buffer]
        end if
    end do

    do row = 1, size(lines)
        buffer = lines(row)
        do col = 1, len_trim(buffer)
            ! print *, "CHAR:", buffer(col:col)
            if (index(numStrs, buffer(col:col)) == 0 .and. buffer(col:col) .ne. ".") then
                if (row > 1) then
                    call findUniqueNum(row - 1, col)  ! N
                    if (col > 1) then
                        call findUniqueNum(row - 1, col - 1)  ! NW
                    end if
                    if (col < len_trim(buffer)) then
                        call findUniqueNum(row - 1, col + 1)  ! NE
                    end if
                end if
                if (row < size(lines)) then
                    call findUniqueNum(row + 1, col)  ! S
                    if (col > 1) then
                        call findUniqueNum(row + 1, col - 1)  ! SW
                    end if
                    if (col < len_trim(buffer)) then
                        call findUniqueNum(row + 1, col + 1)  ! SE
                    end if
                end if
                if (col > 1) call findUniqueNum(row, col - 1)  ! W
                if (col < len_trim(buffer)) call findUniqueNum(row, col + 1)  ! E
            end if
        end do

        ! print *, "END LOOP:", buffer, matches
    end do

    do i = 1, size(matches)
        ! print *, "ADDING: ", matches(i)%val
        total = total + matches(i)%val
    end do
    print *, "PART 1:", total

contains
    logical function part_equal(a, b)
        type(Part), intent(in) :: a, b
        ! print *, "COMPARING: ", a, b
        part_equal = (a%row == b%row) .and. (a%col == b%col) .and. (a%val == b%val)
    end function part_equal

    subroutine findUniqueNum (row, col)
        implicit none
        integer, intent(in) :: row, col
        integer :: i, leftPos, newNum
        integer, parameter :: numLen = 5
        character(len=numLen) :: newNumStr
        type(Part) :: newPart

        newNumStr = ""

        if (index(numStrs, lines(row)(col:col)) /= 0) then
            newNumStr = lines(row)(col:col)
            do i = col + 1, col + numLen  ! To the right
                if (index(numStrs, lines(row)(i:i)) /= 0) then
                    newNumStr = trim(newNumStr) // lines(row)(i:i)
                else
                    exit
                end if
            end do

            leftPos = col
            do i = col - 1, col - numLen, -1  ! To the left
                if (index(numStrs, lines(row)(i:i)) /= 0) then
                    newNumStr = lines(row)(i:i) // trim(newNumStr)
                    leftPos = i
                else
                    exit
                end if
            end do
        end if

        ! print *, "NEWNUM:[", newNumStr, len_trim(newNumStr), "]"
        if (len_trim(newNumStr) > 0) then
            read (newNumStr, *) newNum
            newPart = Part(row, leftPos, newNum)
            if (allocated(matches)) then
                if (.not. any([(matches(i) == newPart, i = 1, size(matches))])) matches = [matches, newPart]
            else
                matches = [newPart]
            end if
        end if
    end subroutine findUniqueNum

end program aoc03a
