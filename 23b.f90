program aoc23b
    implicit none

    type Coord
        integer y, x
    endtype

    type NodeDist
        type(Coord) :: name
        integer :: dist
    endtype

    type NodeListItem
        type(Coord) :: name
        type(NodeDist), allocatable :: nodedists(:)
    endtype

    type NodeQItem
        type(Coord) :: yx, prevXing
        integer :: dist
    endtype NodeQItem

    type PathQItem
        type(Coord) :: yx
        integer:: dist
        type(Coord), allocatable :: visited(:)
        type(NodeDist), allocatable :: allDist(:)
    endtype

    character(len=200), allocatable :: lines(:)
    character(len=200) :: line
    integer :: status, i, k, posNode, posDist, newDist, longest=0
    type(Coord) :: beg, end, dirs(4)
    type(NodeListItem), allocatable :: allNodes(:)
    type(Coord), allocatable :: visited(:)
    type(NodeQItem), allocatable :: nodeQueue(:), neighbors(:)
    type(NodeQItem) :: curr, next
    type(PathQItem), allocatable :: pathQueue(:)
    type(PathQItem) :: cPath
    type(NodeDist) :: pathNext
    type(NodeDist), allocatable :: nDist(:)

    allocate(lines(0))
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit
        lines = [lines, line]
    end do
    
    ! Close out the endings to avoid bounds checking while pathfinding
    beg = Coord(2, 2)
    end = Coord(size(lines) - 1, len_trim(lines(1))-1)
    lines(1)(2:2) = "#"
    lines(size(lines))(len_trim(lines(1))-1:len_trim(lines(1))-1) = "#"
    ! print *, "LINES:"
    ! do i = 1, size(lines)
    !     print *, trim(lines(i))
    ! enddo

    ! First, get a list (graph) of all the intersections on the map (nodes) and distances (edges)
    allNodes = [NodeListItem(beg), NodeListItem(end)]
    allocate(allNodes(1)%nodedists(0))
    allocate(allNodes(2)%nodedists(0))

    allocate(visited(0))
    dirs = [Coord(1,0), Coord(0,1), Coord(-1,0), Coord(0,-1)]
    nodeQueue = [NodeQItem(beg, beg, 0)]

    do while (size(nodeQueue) > 0)
        curr = nodeQueue(size(nodeQueue))
        nodeQueue = nodeQueue(:size(nodeQueue)-1)
        ! print *, "CURR:", curr

        if (any(visited%y == curr%yx%y .and. visited%x == curr%yx%x)) cycle
        visited = [visited, curr%yx]

        allocate(neighbors(0))

        do i=1, size(dirs)
            next = curr
            next%yx%y = next%yx%y + dirs(i)%y
            next%yx%x = next%yx%x + dirs(i)%x
            next%dist = next%dist + 1

            if (lines(next%yx%y)(next%yx%x:next%yx%x) /= '#') then
                neighbors = [neighbors, next]
            endif
        enddo

        if (size(neighbors) > 2) then
            call addNode(curr%yx, curr%prevXing, curr%dist)
        endif

        do i=1, size(neighbors)
            next = neighbors(i)  ! Copy this out, so things don't wig out when deallocating after loop.

            if (size(neighbors) > 2) then
                next%prevXing = curr%yx
                next%dist = 1
            else
                if ((.not. sameCoord(curr%prevXing, next%yx)) .and. any(sameCoord(allNodes%name, next%yx))) then
                    call addNode(next%yx, curr%prevXing, curr%dist + 1)
                endif
            endif

            ! print *, "NEXT:", next%yx, next%prevXing, next%dist
            nodeQueue = [nodeQueue, next]
        enddo

        deallocate(neighbors)
    enddo
    ! print *, "allNodes"
    ! do i=1, size(allNodes)
    !     print *, allNodes(i)%name
    !     do k=1, size(allNodes(i)%nodedists)
    !         print *, "    ", allNodes(i)%nodedists(k)
    !     enddo
    ! enddo

    ! Then, just brute-force DFS through it to get the longest path. Naively, natch.
    pathQueue = [PathQItem(beg, 0, [beg], [NodeDist(beg, 0)])]

    do while (size(pathQueue) > 0)
        cPath = pathQueue(size(pathQueue))
        pathQueue = pathQueue(:size(pathQueue)-1)
        ! print *, "CURR:", cPath%yx

        if (sameCoord(cPath%yx, end) .and. cPath%dist > longest) then
            longest = cPath%dist
            print *, "NEW LONGEST:", longest
            cycle
        endif

        ! Get neighbors and distances
        posNode = 0
        do i=1, size(allNodes)
            if(sameCoord(allNodes(i)%name, cPath%yx)) then
                posNode = i
                exit
            endif
        enddo
        do i=1, size(allNodes(posNode)%nodedists)
            pathNext = allNodes(posNode)%nodedists(i)  ! Get neighbor
            ! print *, "NEIGHBOR:", pathNext%name

            if (.not. any(sameCoord(cPath%visited, pathNext%name))) then  ! Not already visisted...
                posDist = 0
                nDist = cPath%allDist
                do k=1, size(nDist)  ! Get dist of current coord
                    if (sameCoord(nDist(k)%name, cPath%yx)) then
                        posDist = k
                        exit
                    endif
                enddo
                if (posDist == 0) then  ! Create, if doesn't exist
                    nDist = [nDist, NodeDist(cPath%yx, 0)]
                    posDist = size(nDist)
                endif
                newDist = nDist(posDist)%dist + pathNext%dist

                posDist = 0
                do k=1, size(nDist)  ! Get dist of neighbor coord
                    if (sameCoord(nDist(k)%name, pathNext%name)) then
                        posDist = k
                        exit
                    endif
                enddo
                if (posDist == 0) then  ! Create, if doesn't exist
                    nDist = [nDist, NodeDist(pathNext%name, 0)]
                    posDist = size(nDist)
                endif

                if (posDist > 0 .and. newDist >= nDist(posDist)%dist) then
                    nDist(posDist)%dist = newDist

                    pathQueue = [pathQueue, PathQItem( &
                        pathNext%name, &
                        newDist, &
                        [cPath%visited, pathNext%name], &
                        nDist )]
                endif
            endif
        enddo
    enddo

    print *, "PART 2:", longest + 2  ! +2 to account for moving beg and end into new bounds

contains
    elemental logical function sameCoord(a, b)
        type(Coord), intent(in) :: a, b
        if (a%y == b%y .and. a%x == b%x) then
            sameCoord = .true.
        else
            sameCoord = .false.
        endif
    endfunction

    subroutine addNode(new, prev, dist)
        type(Coord), intent(in) :: new, prev
        integer, intent(in) :: dist
        integer :: posNew, posPrev, i

        ! print *, "addNode: ", new, prev, dist
        posNew = 0
        posPrev = 0
        do i=1, size(allNodes)
            if (sameCoord(allNodes(i)%name, new)) posNew = i
            if (sameCoord(allNodes(i)%name, prev)) posPrev = i
            if (posNew > 0 .and. posPrev > 0) exit
        enddo

        if (posNew == 0) then
            allNodes = [allNodes, NodeListItem(new)]
            allNodes(size(allNodes))%nodedists = [NodeDist(prev, dist)]
        else
            allNodes(posNew)%nodedists = [allNodes(posNew)%nodedists, NodeDist(prev, dist)]
        endif

        if (posPrev == 0) then
            allNodes = [allNodes, NodeListItem(prev)]
            allNodes(size(allNodes))%nodedists = [NodeDist(new, dist)]
        else
            allNodes(posPrev)%nodedists = [allNodes(posPrev)%nodedists, NodeDist(new, dist)]
        endif
    endsubroutine
end program aoc23b