program aoc07a
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
        call play_array_push(allPlays, Play(currCards, currBid, getType(currCards)))
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
                call play_array_push(currRank, allPlays(k))

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

    print *, "PART 1:", total

contains
    subroutine play_array_push(arr, item)
        type(Play), dimension(:), allocatable, intent(inout) :: arr
        type(Play), intent(in) :: item
        if (allocated(arr)) then
            arr = [arr, item]
        else
            arr = [item]
        endif
    endsubroutine

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
        cards = replace_text(cards, "J", "W")
        cards = replace_text(cards, "T", "V")
    endsubroutine

    integer function getType (cards)
        use LexicalSort
        implicit none

        character(len=5), intent(in) :: cards
        character, dimension(5) :: cardArray
        integer, dimension(5) :: sortedIndexes
        integer, dimension(5) :: grouped
        integer :: i, k, groupLen
        
        do i = 1, len_trim(cards)
            cardArray(i) = cards(i:i)
        enddo
        call sort(cardArray, sortedIndexes)
        ! print *, "CARDS: ", cards, " ARRAY: ", cardArray, " INDEXES: ", sortedIndexes

        grouped = 1
        groupLen = 1
        i = 1
        do while (i <= size(cardArray))
            do k = i + 1, size(cardArray)
                if (cardArray(k) == cardArray(i)) then
                    grouped(groupLen) = grouped(groupLen) + 1
                    i = i + 1
                else
                    groupLen = groupLen + 1
                    exit
                endif
            enddo
            ! print *, "CARDS: ", cardArray, " GROUP:", grouped(groupLen - 1)
            i = i + 1
        enddo

        if (any(grouped == 5)) then
            gettype = 7
        else if (any(grouped == 4)) then
            gettype = 6
        else if (any(grouped == 3) .and. (any(grouped == 2))) then
            gettype = 5
        else if (any(grouped == 3)) then
            gettype = 4
        else if (findloc(grouped, 2, dim=1) /= findloc(grouped, 2, dim=1, back=.true.)) then
            gettype = 3
        else if (any(grouped == 2)) then
            gettype = 2
        else
            gettype = 1
        endif
    endfunction
end program aoc07a