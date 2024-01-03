program aoc20a
    implicit none

    type Mod
        integer :: id
        character(len=2) :: name, type
        logical :: state
        integer, allocatable :: dest(:), iOpts(:)  ! Order must match order of initialization!
        logical, allocatable :: lOpts(:)
    endtype

    type qItem
        integer :: curr, prev 
        logical :: highPulse
    endtype
    
    character(len=50) :: line
    integer :: status, i, k, newId, pos, commas, iBroadcaster, highNum=0, lowNum=0
    character(len=2), allocatable :: lookup(:), sDest(:)
    integer, allocatable :: nDest(:)
    type(Mod), allocatable, target :: allMods(:)
    type(qItem), allocatable :: queue(:)

    allocate(lookup(0))
    allocate(allMods(0))

    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit
        ! print *, "LINE:[", line, "]"

        newId = getId(line(2:3))  ! Also creates the new Mod
        allMods(newId)%type = line(1:1)

        pos = index(line, " -") + 3
        commas = count([(line(i:i) == ",", i=1, len_trim(line))])
        allocate(sDest(commas+1))
        read (line(pos:), *) sDest

        nDest = [(getId(sDest(i)), i=1, size(sDest))]
        allMods(newId)%dest = nDest

        allocate(allMods(newId)%iOpts(0))
        allocate(allMods(newId)%lOpts(0))

        if (line(2:3) == "ro") iBroadcaster = newId

        deallocate(sDest)
        deallocate(nDest)
    enddo
    ! print *, "ALLMODS: ", allMods%name, " : ", allMods%type
    do i=1, size(allMods)  ! Map every % back to a conjunction to remember previous high/low state.
        if (.not. allocated(allMods(i)%dest)) then  ! For the final (rx) ones that don't have a literal dest.
            allocate(allMods(i)%dest(0))
        endif
        do k=1, size(allMods(i)%dest)
            if (allMods(allMods(i)%dest(k))%type == "&") then
                allMods(allMods(i)%dest(k))%iOpts = [allMods(allMods(i)%dest(k))%iOpts, i]
                allMods(allMods(i)%dest(k))%lOpts = [allMods(allMods(i)%dest(k))%lOpts, .false.]
            endif
        enddo
    enddo
    ! print *, "ALLMODS"
    ! do i=1, size(allMods)
    !     print *, allMods(i)%id, allMods(i)%name, " : ", allMods(i)%type, " : ", allMods(i)%state, " : ", allMods(i)%dest
    !     if (allocated(allMods(i)%lOpts) .and. size(allMods(i)%lOpts) > 0) then
    !         print *, "    LOPTS", allMods(i)%iOpts, allMods(i)%lOpts
    !     endif
    ! enddo

    do i=1, 1000 
        allocate(queue(0))
        call sendQueuePulse(0, .false.)
        call startModules()
        ! print *, "[", i, "]", lowNum, highNum
        deallocate(queue)
    enddo

    print *, "PART 1:", lowNum * highNum

contains
    integer function getId(inName)
        character(len=2), intent(in) :: inName

        do i=1, size(lookup)
            if (lookup(i) == trim(inName)) then
                getId = i
                return
            endif
        enddo
        ! print *, "CREATING:", size(lookup) + 1, ": ", inName
        lookup = [lookup, trim(inName)]
        allMods = [allMods, Mod(size(lookup), trim(inName), "", .false.)]
        getId = size(lookup)
    endfunction

    subroutine sendQueuePulse(curr, highPulse)
        integer, intent(in) :: curr
        logical :: highPulse
        integer :: i

        ! print *, "PULSE:", size(queue), lookup(curr), highPulse
        if (curr == 0) then
            lowNum = lowNum + 1
            queue = [queue, qItem(iBroadcaster, 0, highPulse)]
        else
            do i=1, size(allMods(curr)%dest)
                if (highPulse) then
                    highNum = highNum + 1
                else
                    lowNum = lowNum + 1
                endif
                queue = [queue, qItem(allMods(curr)%dest(i), curr, highPulse)]
            enddo
        endif
    endsubroutine

    subroutine startModules()
        integer :: i, k
        type(Mod), pointer :: curr

        i = 1
        do while (i <= size(queue))
            ! print *, "Q:", lookup(queue(i)%curr), " ", lookup(queue(i)%prev), queue(i)%highPulse
            curr => allMods(queue(i)%curr)

            if (curr%type == "%") then
                if (.not. queue(i)%highPulse) then
                    curr%state = .not. curr%state
                    call sendQueuePulse(curr%id, curr%state)
                endif
            else if (curr%type == "&") then
                do k=1, size(curr%iOpts)  ! Use the id against the iOpts to find the proper position in the lOpts
                    if (curr%iOpts(k) == queue(i)%prev) then
                        curr%lOpts(k) = queue(i)%highPulse
                        exit
                    endif
                enddo
                if (size(curr%lOpts) == 0 .or. all(curr%lOpts)) then
                    call sendQueuePulse(curr%id, .false.)
                else
                    call sendQueuePulse(curr%id, .true.)
                endif
            else
                call sendQueuePulse(curr%id, queue(i)%highPulse);
            endif

            i = i + 1
        enddo
    endsubroutine
end program aoc20a