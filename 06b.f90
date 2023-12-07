program aoc06b
    implicit none
    
    character(len=50) :: values
    character(len=10) :: key
    character(len=25) :: charBuffer
    integer(kind=8) :: time, distance, speed, newDistance, breakers, total
    integer :: i 

    charBuffer = ''
    read (*, '(a10, a)') key, values
    values = trim(adjustl(values))
    do i = 1, len_trim(values)
        if (lle('0', values(i:i)) .and. lge('9', values(i:i))) then
            charBuffer = trim(charBuffer) // values(i:i)
        endif
    enddo
    if (len_trim(charBuffer) > 0) read (charBuffer, *) time

    charBuffer = ''
    read (*, '(a10, a)') key, values
    values = trim(adjustl(values))
    do i = 1, len_trim(values)
        if (lle('0', values(i:i)) .and. lge('9', values(i:i))) then
            charBuffer = trim(charBuffer) // values(i:i)
        endif
    enddo
    if (len_trim(charBuffer) > 0) read (charBuffer, *) distance
    ! print *, "TIMES:", time, "DIST:", distance

    total = 1
    speed = 1
    do while (speed <= time - 1)
        newDistance = speed * (time - speed)
        if (newDistance > distance) exit
        speed = speed + 1
    enddo

    breakers = (time + 1) - (speed * 2);
    total = total * breakers

    print *, "PART 1:", total
end program aoc06b