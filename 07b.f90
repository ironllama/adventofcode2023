program aoc07b
    use LexicalSort
    implicit none

    type Play
        character(len=5) :: cards
        integer :: bid, type
    endtype

    integer :: status, currBid, i, k, m, rank, total
    character(len=20) :: line
    character(len=5) :: currCards
    type(Play), dimension(:), allocatable :: allPlays
    type(Play), dimension(:), allocatable :: currRank
    character(len=5), dimension(:), allocatable :: currRankCards
    integer, dimension(:), allocatable :: currRankCardsOrder

    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        read (line, *) currCards, currBid
        call getLexical(currCards)
        if (allocated(allPlays)) then
            allPlays = [allPlays, Play(currCards, currBid, getType(currCards))]
        else
            allPlays = [Play(currCards, currBid, getType(currCards))]
        endif
    enddo
    ! print *, "ALL:"
    ! do i = 1, size(allPlays)
    !     print *, allPlays(i)
    ! enddo

    total = 0
    rank = 1
    do i = 1, 7  ! Go through all the types
        do k = 1, size(allPlays)  ! Collect all hands of each types
            if (allPlays(k)%type == i) then
                if (allocated(currRank)) then
                    currRank = [currRank, allPlays(k)]
                else
                    currRank = [allPlays(k)]
                endif

                if (allocated(currRankCards)) then  ! For sorting, later...
                    currRankCards = [currRankCards, allPlays(k)%cards]
                else
                    currRankCards = [allPlays(k)%cards]
                endif
            endif
        enddo
        ! print *, "CURR:", currRankCards

        if (allocated(currRank)) then
            allocate(currRankCardsOrder(size(currRankCards)));
            call sort(currRankCards, currRankCardsOrder)
            ! print *, "SORTED:", currRankCardsOrder
            ! do k = 1, size(currRank)
            !     print *, currRank(k)
            ! enddo
            ! do k = 1, size(currRankCards)
            !     print *, currRankCards(k)
            ! enddo

            do k = 1, size(currRankCardsOrder)
                m = currRankCardsOrder(k)
                currBid = currRank(m)%bid
                total = total + (currBid * rank)
                ! print *, "ADDED: ", m, currRank(m), currBid, rank, total

                rank = rank + 1
            enddo

            deallocate(currRank)
            deallocate(currRankCards)
            deallocate(currRankCardsOrder)
        endif
    enddo

    if (allocated(allPlays)) deallocate(allPlays);
    print *, "PART 1:", total

contains
    function replace_text (s, text, rep) result(outs)
        character(*), intent(in) :: s, text, rep
        integer :: i
        character(len=5) :: outs

        outs = s
        do
            i = index(outs, text(:len_trim(text)))
            if (i == 0) exit
            outs = outs(:i-1) // rep(:len_trim(rep)) // outs(i + len_trim(text):)
        end do
    endfunction

    subroutine getLexical(cards)
        character(len=5), intent(inout) :: cards

        cards = replace_text(cards, "A", "Z")
        cards = replace_text(cards, "K", "Y")
        cards = replace_text(cards, "Q", "X")
        cards = replace_text(cards, "J", "1")
        cards = replace_text(cards, "T", "V")
    endsubroutine

    integer function getType (cards)
        use LexicalSort
        implicit none

        character(len=5), intent(in) :: cards
        character, dimension(5) :: cardArray
        integer, dimension(5) :: sortedIndexes
        integer, dimension(:), allocatable :: grouped, sortedGroup
        character(len=5), dimension(:), allocatable :: groupedStr
        character(len=5) :: charBuffer
        integer :: i, k, count, numJs, tempNum
        character, dimension(:), allocatable :: noJs
        
        if (cards /= "11111") then
            do i = 1, len_trim(cards)
                cardArray(i) = cards(i:i)
            enddo
            call sort(cardArray, sortedIndexes)
            ! print *, "CARDS: ", cards, " ARRAY: ", cardArray, " INDEXES: ", sortedIndexes

            do i = 1, size(cardArray)
                if (cardArray(i) /= '1') then
                    if (allocated(noJs)) then
                        noJs = [noJs, cardArray(i)]
                    else
                        noJs = [cardArray(i)]
                    endif
                endif
            enddo
            numJs = size(cardArray) - size(noJs)

            i = 1
            do while (i <= size(noJs))
                count = 1
                do k = i + 1, size(noJs)
                    if (noJs(k) == noJs(i)) then
                        count = count + 1
                        i = i + 1
                    else
                        exit
                    endif
                enddo

                if (allocated(grouped)) then
                    grouped = [grouped, count]
                else
                    grouped = [count]
                endif
                ! print *, "CARDS: ", noJs, " GROUP:", grouped(groupLen - 1)
                i = i + 1
            enddo
            ! print *, "GROUPED:[", grouped, "]"

            if (allocated(grouped)) then
                charBuffer = ''
                do i = 1, size(grouped)
                    write (charBuffer, '(i0)') grouped(i)
                    if (allocated(groupedStr)) then
                        groupedStr = [groupedStr, charBuffer]
                    else
                        groupedStr = [charBuffer]
                    endif
                enddo
                call sort(groupedStr, sortedIndexes)

                do i = size(groupedStr), 1, -1
                    read (groupedStr(i), *) tempNum
                    if (allocated(sortedGroup)) then
                        sortedGroup = [sortedGroup, tempNum]
                    else
                        sortedGroup = [tempNum]
                    endif
                enddo
            endif

            count = 5
            if (allocated(sortedGroup) .and. size(sortedGroup) > 0) count = sortedGroup(1) + numJs 

            ! print *, "CARD: ", cards, " GROUPS:", sortedGroup, "NUMJS:", numJs, "COUNT:", count
            if (count == 5) then
                gettype = 7
            else if (count == 4) then
                gettype = 6
            else if (count == 3 .and. sortedGroup(2) == 2) then
                gettype = 5
            else if (count == 3) then
                gettype = 4
            else if (count == 2 .and. sortedGroup(2) == 2) then
                gettype = 3
            else if (count == 2) then
                gettype = 2
            else
                gettype = 1;
            endif
        else
            gettype = 7
        endif
        ! print *, "RETURN:", gettype

        if (allocated(groupedStr)) deallocate(groupedStr)
        if (allocated(grouped)) deallocate(grouped)
        if (allocated(noJs)) deallocate(noJs)
        if (allocated(sortedGroup)) deallocate(sortedGroup)
    endfunction
end program aoc07b