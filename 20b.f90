program aoc20b
    implicit none

    type Conf
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
    integer :: status, i, k, newId, pos, commas, iBroadcaster, highNum=0, lowNum=0, loopNum=1
    integer(kind=8) :: total
    character(len=2), allocatable :: lookup(:), sDest(:), interested(:)
    integer, allocatable :: nDest(:), interestedPos(:)
    type(Conf), allocatable, target :: allMods(:)
    type(qItem), allocatable :: queue(:)
    logical, allocatable :: interestedActive(:)

    allocate(lookup(0))
    allocate(allMods(0))

    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit
        ! print *, "LINE:[", line, "]"

        newId = getId(line(2:3))  ! Also creates the new Conf
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

    interested = ["gl", "gk", "hr", "nr"]  ! Based on output, these seem to be some sort of bit group
    allocate(interestedPos(size(interested)))
    interestedPos = 0
    allocate(interestedActive(size(interested)))
    interestedActive = .false.

    do while(.true.)
        allocate(queue(0))
        
        call sendQueuePulse(0, .false.)
        if (startModules()) exit

        deallocate(queue)
        loopNum = loopNum + 1
    enddo
    ! print *, "POS: ", interestedPos

    total = int8(interestedPos(1))
    do i=2, size(interestedPos)
        total = lcm(total, int8(interestedPos(i)))
    enddo

    print *, "PART 2:", total

contains
    ! Adapted from: https://stackoverflow.com/questions/47047682/least-common-multiple-of-an-array-values-using-euclidean-algorithm
    recursive function gcd (a, b) result (out)
        integer(kind=8), intent(in) :: a, b
        integer(kind=8) :: out
        integer(kind=8) :: modres
        if (a > 0) then
            ! out = gcd(mod(b, a), a)
            modres = mod(b, a)
            out = gcd(modres, a)
        else
            out = b
        endif
    endfunction

    recursive function lcm (a, b) result (out)
        integer(kind=8), intent(in) :: a, b
        integer(kind=8) :: out
        out = (a * b) / gcd(a, b)
    endfunction

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
        allMods = [allMods, Conf(size(lookup), trim(inName), "", .false.)]
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

    logical function startModules()
        integer :: i, k, m
        type(Conf), pointer :: curr, interest

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

                ! Track the interesting bits of the input
                do k=1, size(interested)
                    interest => allMods(getId(interested(k)))
                    ! print *, "INTEREST:", interest%name
                    if (interestedActive(k) .and. all(.not. interest%lOpts)) then
                        if (interestedPos(k) == 0) then
                            interestedPos(k) = loopNum
                            if (all(interestedPos /= 0)) then
                                startModules = .true.
                                return
                            endif
                        else
                        endif
                    else if (all(interest%lOpts)) then
                        interestedActive(k) = .true.
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
        startModules = .false.
    endfunction
end program aoc20b