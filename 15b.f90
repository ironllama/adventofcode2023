program aoc15b
    implicit none

    type Lens
        character(len=10), allocatable :: lenses(:)
    endtype

    character(len=10) :: label
    character :: char, oper, fStr
    integer :: status, posBox, i, total, focal, k
    type(Lens) :: boxes(256)
    logical :: found

    label = ""
    oper = ""
    fStr = ""
    do
        read (*, '(a)', advance="no", iostat=status) char
        if (char == "," .or. is_iostat_end(status)) then
            ! print *, ">>>>>>>>>>>>> STEP:", label, oper, ", ", fStr
            posBox = hash(label)
            if (oper == "-") then
                if (allocated(boxes(posBox)%lenses)) then
                    do i = 1, size(boxes(posBox)%lenses)
                        if (index(boxes(posBox)%lenses(i), trim(label)) /= 0) then
                            ! print *, "DELETE"
                            boxes(posBox)%lenses = [boxes(posBox)%lenses(1:i-1), boxes(posBox)%lenses(i+1:)]
                            exit
                        endif
                    enddo
                endif
            else
                if (allocated(boxes(posBox)%lenses)) then
                    found = .false.
                    do i = 1, size(boxes(posBox)%lenses)
                        if (index(boxes(posBox)%lenses(i), trim(label)) /= 0) then
                            ! print *, "REPLACE"
                            boxes(posBox)%lenses(i:i) = trim(label) // " " // fStr
                            found = .true.
                            exit
                        endif
                    enddo
                    if (.not. found) then
                        ! print *, "ADD"
                        boxes(posBox)%lenses = [boxes(posBox)%lenses, trim(label) // " " // fStr]
                    endif
                else
                    ! print *, "ADD"
                    boxes(posBox)%lenses = [trim(label) // " " // fStr]
                endif
            endif

            label = ""
            oper = ""
            fStr = ""
        else
            if (char == "-" .or. char == "=") then
                oper = char
            else if (oper == "") then
                label = trim(label) // char
            else
                fStr = char
            endif
        endif

        if (is_iostat_end(status)) exit
    enddo

    total = 0
    ! print *, "BOXES:"
    do i = 1, size(boxes)
        if (allocated(boxes(i)%lenses) .and. size(boxes(i)%lenses) > 0) then
            do k = 1, size(boxes(i)%lenses)
                read (boxes(i)%lenses(k), *) label, focal
                ! print *, "BOX:", i, "LENSES:", boxes(i)%lenses(k), "LABEL:", label, "FOCAL:", focal
                total = total + (i * k * focal)
            enddo
        endif
    enddo

    print *, "PART 2:", total
    
contains
    integer function hash(label)
        character(len=*), intent(in) :: label
        integer :: i

        hash = 0
        do i = 1, len_trim(label)
            hash = mod((hash + ichar(label(i:i))) * 17, 256)
        enddo
        hash = hash + 1  ! Our boxes are 1-indexed
    endfunction
end program aoc15b