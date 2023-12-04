program aoc04b
    implicit none
    character(len=200), dimension(:), allocatable :: lines
    character(len=200) :: line
    character :: char
    character(len=5) :: numBuffer
    integer :: status, total, startWinningPlayingNum, currNum, i, k, m
    integer, dimension(:), allocatable :: winning, won
    integer, dimension(:), allocatable :: numCards

    ! Read all the lines in.
    status = 0
    do
        read(*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit
        if (allocated(lines)) then
            lines = [lines, line]
        else
            lines = [line]
        end if
    end do
    allocate(numCards(size(lines)))  ! Just so we can get future num of cards. Yeesh.
    numCards = 1
    ! print *, "LINES:", size(lines), numCards

    ! Then process by char, by line.
    total = 0
    startWinningPlayingNum = 1
    numBuffer = ''

    do i = 1, size(lines)
        do k = 1, len_trim(lines(i)) + 1  ! Playing with fire? Maybe.
            if (k == len_trim(lines(i)) + 1) then
                char = ' '
            else
                char = lines(i)(k:k)
            end if
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
        end do

        if (allocated(winning)) deallocate(winning)
        if (allocated(won)) then
            ! print *, i, "WON:", size(won)
            do m = 1, size(won)
                numCards(i + m) = numCards(i + m) + numCards(i)
            end do
            deallocate(won)
        end if

        ! Prep for next line.
        startWinningPlayingNum = 1
        numBuffer = ''
    end do

    print *, "PART 2:", sum(numCards)
end program aoc04b