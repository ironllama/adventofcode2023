program aoc25a
    implicit none

    type Vertex
        character(len=3) :: name
        character(len=3), allocatable :: edges(:), subverts(:)
    endtype
    
    character(len=50) :: line
    integer :: status, num, numCurr, i, numEdges = 0, total = 0
    character(len=3), allocatable :: tokens(:)
    type(Vertex) :: next
    type(Vertex), allocatable :: graph(:), cGraph(:)

    allocate(graph(0))
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        ! print *, "LINE: ", line
        num = count([(line(i:i) == " ", i = 1, len_trim(line))])
        allocate(tokens(num + 1))
        read(line, *) tokens

        do i = 1, size(tokens)
            num = getVertex(tokens(i), graph)  ! Create new vertex, if it doesn't exist
            if (num == 0) then
                next = Vertex(tokens(i))
                allocate(next%edges(0))
                allocate(next%subverts(0))
                graph = [graph, next]
                num = size(graph)
            else
                next = graph(num)
            endif

            if (i == 1) then
                numCurr = num
            else  ! Put in the edges
                graph(numCurr)%edges = [graph(numCurr)%edges, tokens(i)]
            endif
        enddo

        deallocate(tokens)
    enddo
    ! print *, "GRAPH: "
    ! do i = 1, size(graph)
    !     print *, graph(i)%name, ": [", graph(i)%edges, "] [", graph(i)%subverts, "]"
    ! enddo

    do while (numEdges /= 3)
        cGraph = graph

        call karger(cGraph, 2)
        numEdges = size(cGraph(1)%edges) + size(cGraph(2)%edges)
        print *, "NUM EDGES:", numEdges
    enddo

    total = (size(cGraph(1)%subverts) + 1) * (size(cGraph(2)%subverts) + 1)
    print *, "PART 1:", total

contains
    subroutine karger (graph, t)
        type(Vertex), allocatable, target, intent(inout) :: graph(:)
        integer, intent(in) :: t
        type(Vertex), pointer :: src, dest
        character(len=3) :: vName
        integer :: i, pos

        do while (size(graph) > t)
            src => graph(ceiling(rand() * size(graph)))
            if (size(src%edges) == 0) cycle
            vName = src%edges(ceiling(rand() * size(src%edges)))
            pos = getVertex(vName, cGraph)
            dest => graph(pos)
            ! print *, "SRC: ", src%name, " E[", src%edges, "] V[", src%subverts, "]"
            ! print *, "DST: ", vName, "/", dest%name, " E[", dest%edges, "] V[", dest%subverts, "]"

            src%subverts = [src%subverts, dest%name, dest%subverts]  ! Combine subverts into src
            src%edges = pack(src%edges, src%edges /= dest%name)  ! Remove dest name from src edges
            do i = 1, size(dest%edges)  ! Update the src edges
                if (dest%edges(i) /= src%name) src%edges = [src%edges, dest%edges(i)]
            enddo
            ! print *, "TRANSFER: E[", src%edges, "] V[", src%subverts, "]"

            do i = 1, size(graph)
                where (graph(i)%edges == dest%name)
                    graph(i)%edges = src%name
                endwhere
            enddo

            graph = pack(graph, graph%name /= dest%name)  ! Delete dest vertex
            ! print *, "GRAPH: "
            ! do i = 1, size(graph)
            !     print *, graph(i)%name, ": [", graph(i)%edges, "] [", graph(i)%subverts, "]"
            ! enddo
        enddo
    endsubroutine

    integer function getVertex(inName, graph)
        type(Vertex), allocatable, intent(inout) :: graph(:)
        character(len=3), intent(in) :: inName
        integer :: i

        do i = 1, size(graph)
            if (graph(i)%name == inName) then
                getVertex = i
                return
            endif
        enddo
        getVertex = 0
    endfunction
end program aoc25a