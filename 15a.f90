program aoc15a
    implicit none
    character(len=10) :: line
    character :: char
    integer :: status, temp, total

    line = ""
    total = 0
    do
        read (*, '(a)', advance="no", iostat=status) char
        if (char == "," .or. is_iostat_end(status)) then
            temp = hash(line);
            total = total + temp
            ! print *, "LINE:", line, "HASH:", temp

            line = ""
        else
            line = trim(line) // char
        endif

        if (is_iostat_end(status)) exit
    enddo

    print *, "PART 1:", total
contains
    integer function hash(label)
        character(len=*), intent(in) :: label
        integer :: i

        hash = 0
        do i = 1, len_trim(label)
            hash = mod((hash + ichar(label(i:i))) * 17, 256)
        enddo
    endfunction
end program aoc15a