program aoc19a
    implicit none

    type Workflow
        character(len=10) :: name
        character(len=20), allocatable :: rules(:)
    endtype

    type(Workflow), allocatable :: allWF(:)
    character(len=50) :: line
    character(len=20), allocatable :: tempRules(:)
    character(len=20) :: ratingStrs(4)
    integer :: status, i, pos, num, ratings(4), total
    logical :: wfFilled=.false.
    
    total = 0
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        if (len_trim(line) > 0) then
            if (.not. wfFilled) then
                pos = index(line, "{")
                ! rulesStr = line(pos+1:len_trim(line)-1)
                num = count([(line(i:i) == ",", i = 1, len_trim(line))])
                allocate(tempRules(num+1))
                read(line(pos+1:len_trim(line)-1), *) tempRules

                if (allocated(allWF)) then
                    allWF = [allWF, Workflow(line(1:pos-1), tempRules)]
                else
                    allWF = [Workflow(line(1:pos-1), tempRules)]
                endif

                deallocate(tempRules)
            else
                ratings = 0
                read(line(2:len_trim(line)-1), *) ratingStrs
                do i = 1, size(ratingStrs)
                    read (ratingStrs(i)(3:), *) ratings(i)
                enddo

                if (approvalCode(ratings, "in        ") == 'A') then
                    total = total + SUM(ratings)
                endif
            endif
        else
            wfFilled = .true.
        endif
    enddo
    ! print *, "WF:"
    ! do i = 1, size(allWF)
    !     print *, allWF(i)%name, allWF(i)%rules
    ! enddo

    print *, "PART 1:", total
contains
    recursive character(len=10) function approvalCode(ratings, wf) result(code)
        integer, intent(in) :: ratings(4)
        character(len=10), intent(in) :: wf
        character(len=10) :: next, result
        character :: item, oper, allItems(4)= ["x", "m", "a", "s"]
        integer :: i, pos, amt
        type(Workflow) :: curr

        curr = allWF(findloc(allWF%name, wf, dim=1))
        ! print *, "CURR: ", curr%rules

        do i = 1, size(curr%rules)
            pos = index(curr%rules(i), ":")
            ! print *, "PROCESS:", curr%rules(i), pos
            if (pos == 0) then
                result = curr%rules(i)
            else
                item = curr%rules(i)(1:1)
                oper = curr%rules(i)(2:2)
                pos = index(curr%rules(i), ":")
                read (curr%rules(i)(3:pos-1), *) amt
                next = curr%rules(i)(pos+1:)

                pos = findloc(allItems, item, dim=1)

                if (oper == ">" .and. ratings(pos) > amt) then
                    result = next
                    exit
                else if (oper == "<" .and. ratings(pos) < amt) then 
                    result = next
                    exit
                endif
            endif
        enddo

        if (result == "A") then
            code = "A"
        else if (result == "R") then
            code = "R"
        else
            code = approvalCode(ratings, result)
        endif
    endfunction
end program aoc19a