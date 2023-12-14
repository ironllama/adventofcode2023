program aoc14a
    implicit none
    
    character(len=150), allocatable :: lines(:)
    character(len=150) :: line
    integer :: status, r, c, bottom, total

    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit
        if (allocated(lines)) then
            lines = [lines, line]
        else
            lines = [line]
        endif
    enddo
    ! print *, "LINES:"
    ! do r = 1, size(lines)
    !     print *, trim(lines(r))
    ! enddo

    do c = 1, len_trim(lines(1))
        bottom = 1
        do r = 1, size(lines)
            if (lines(r)(c:c) == "O") then
                if (r > bottom) then
                    lines(bottom)(c:c) = "O"
                    lines(r)(c:c) = "."
                endif
                bottom = bottom + 1
            else if (lines(r)(c:c) == "#") then
                bottom = r + 1
            endif
        enddo
    enddo
    ! print *, "LINES:"
    ! do r = 1, size(lines)
    !     print *, trim(lines(r))
    ! enddo

    total = 0
    do r = 1, size(lines)
        ! total = total + (count([(lines(r)(c:c) == "O", c = 1, len_trim(lines(r)))]) * (size(lines) - r + 1))
        ! Same as above, but without the temporary array... not sure which is better?
        do c = 1, len_trim(lines(r))
            if (lines(r)(c:c) == "O") total = total + (size(lines) - r + 1)
        enddo
    enddo

    print *, "PART 1:", total

end program aoc14a