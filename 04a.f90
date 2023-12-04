program aoc04a
    implicit none
    character :: char
    character(len=5) :: numBuffer
    integer :: status, total, startWinningPlayingNum, currNum
    integer, dimension(:), allocatable :: winning, won

    total = 0
    status = 0

    startWinningPlayingNum = 1
    numBuffer = ''

    do
        read (*, '(a)', advance='no', iostat=status) char
        if (is_iostat_end(status)) char = ' '
        ! print *, "CHAR:", char
        if (char == ':') then
            startWinningPlayingNum = 2
        else if (char == ' ' .and. len_trim(numBuffer) > 0) then
            ! print *, "NEW NUM:", numBuffer
            read (numBuffer, *) currNum
            numBuffer = '';
            if (startWinningPlayingNum == 2) then
                if (allocated(winning)) then
                    winning = [winning, currNum]
                else
                    winning = [currNum]
                end if
            else
                if (any(winning == currNum)) then
                    if (allocated(won)) then
                        won = [won, currNum]
                    else
                        won = [currNum]
                    end if
                end if
            end if
        else if (char == '|') then
            startWinningPlayingNum = 3
        else if (char /= ' ' .and. startWinningPlayingNum /= 1) then
            numBuffer = trim(numBuffer) // char
        else
            ! print *, 'SKIP, WILL ROBINSON[', char, ']'
        end if

        if (status /= 0) then  ! Both end and eor
            ! print *, "EOR:", winning, "WON:", won
            ! print *, "SIZE:", size(won), "WON:[", won, "]"
            if (allocated(won)) total = total + (2**(size(won) - 1))

            ! Prep for next line.
            startWinningPlayingNum = 1
            numBuffer = ''
            if (allocated(winning)) deallocate(winning)
            if (allocated(won)) deallocate(won)
        end if

        if (is_iostat_end(status)) exit
    end do

    print *, "PART 1:", total
end program aoc04a