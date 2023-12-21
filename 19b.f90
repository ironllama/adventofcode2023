program aoc19b
    implicit none

    type Rule
        character :: type
        character :: oper
        integer :: amt
        character(len=20) :: next
    endtype

    type Workflow
        character(len=10) :: name
        type(Rule), allocatable :: rules(:)
    endtype

    type(Workflow), allocatable :: allWF(:)
    character(len=50) :: line
    character(len=20), allocatable :: tempRulesStr(:)
    type(Rule), allocatable :: tempRules(:)
    character(len=10) :: next
    character :: item, oper
    integer :: status, i, bracketPos, num, pos, amt
    integer(kind=8) :: total
    logical :: wfFilled = .false.
    type(Rule), allocatable :: paths(:)
    
    total = 0
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        if (len_trim(line) > 0) then
            if (.not. wfFilled) then
                bracketPos = index(line, "{")
                ! rulesStr = line(bracketPos+1:len_trim(line)-1)
                num = count([(line(i:i) == ",", i = 1, len_trim(line))])  ! Num of commas
                allocate(tempRulesStr(num+1))
                read(line(bracketPos+1:len_trim(line)-1), *) tempRulesStr

                !!! Parse each rule upfront, to make it easier down the line !!!
                do i = 1, size(tempRulesStr)
                    pos = index(tempRulesStr(i), ":")
                    item = ""
                    oper = ""
                    amt = 0
                    next = ""
                    if (pos == 0) then
                        next = trim(tempRulesStr(i))
                    else
                        item = tempRulesStr(i)(1:1)
                        oper = tempRulesStr(i)(2:2)
                        pos = index(tempRulesStr(i), ":")
                        read (tempRulesStr(i)(3:pos-1), *) amt
                        next = tempRulesStr(i)(pos+1:)
                    endif

                    if (allocated(tempRules)) then
                        tempRules = [tempRules, Rule(item, oper, amt, next)]
                    else
                        tempRules = [Rule(item, oper, amt, next)]
                    endif
                enddo

                if (allocated(allWF)) then
                    allWF = [allWF, Workflow(line(1:bracketPos-1), tempRules)]
                else
                    allWF = [Workflow(line(1:bracketPos-1), tempRules)]
                endif

                deallocate(tempRulesStr)
                deallocate(tempRules)
            endif
        else
            wfFilled = .true.
        endif
    enddo
    ! print *, "WF:"
    ! do i = 1, size(allWF)
    !     print *, allWF(i)%name, "RULES:", size(allWF(i)%rules), ": ", allWF(i)%rules
    ! enddo

    allocate(paths(0))
    call approvalCode(paths, "in        ");
    deallocate(paths)

    print *, "PART 2:", total

contains
    recursive subroutine approvalCode(paths, wf)
        type(Rule), allocatable, intent(inout) :: paths(:)
        character(len=10), intent(in) :: wf
        integer :: i
        type(Workflow) :: curr
        type(Rule), allocatable :: newPaths(:)

        curr = allWF(findloc(allWF%name, wf, dim=1))
        ! print *, "CURR: ", curr%rules

        do i = 1, size(curr%rules)
            if (curr%rules(i)%oper == "") then
                if (curr%rules(i)%next == "A") then
                    ! print *, "PATHS: ", paths
                    call countPoss(paths)
                else if (curr%rules(i)%next /= "R") then
                    call approvalCode(paths, curr%rules(i)%next)
                endif
            else
                newPaths = paths

                if (curr%rules(i)%next == "A") then
                    newPaths = [newPaths, curr%rules(i)]
                    ! print *, "NEWPATHS: ", newPaths
                    call countPoss(newPaths)
                else if (curr%rules(i)%next /= "R") then
                    newPaths = [newPaths, curr%rules(i)]
                    call approvalCode(newPaths, curr%rules(i)%next)
                endif

                if (curr%rules(i)%oper == ">") then
                    paths = [paths, Rule(curr%rules(i)%type, "<", curr%rules(i)%amt + 1, "")]
                else
                    paths = [paths, Rule(curr%rules(i)%type, ">", curr%rules(i)%amt - 1, "")]
                endif
            endif
        enddo
    endsubroutine

    subroutine countPoss(paths)
        type(Rule), allocatable, intent(in) :: paths(:)
        integer :: i, k, nameTotal, lastNum
        integer(kind=8) :: subTotal
        character :: lastOper, types(4) = ["x", "m", "a", "s"]
        type(Rule), allocatable :: curr(:)

        ! print *, "COUNTPOSS:"
        ! do i = 1, size(paths)
        !     print *, paths(i)
        ! enddo
        subTotal = 1
        do i = 1, size(types)
            curr = pack(paths, types(i) == paths(:)%type)
            call quicksort(curr, 1, size(curr))

            curr = [curr, Rule("", "<", 4001, "")]

            nameTotal = 0
            lastOper = ""
            lastNum = 1
            do k = 1, size(curr)
                if (curr(k)%oper /= "") then
                    if (curr(k)%oper == "<") then
                        if (lastOper /= "<") then
                            ! print *, "RANGE: ", lastNum, " - ", curr(k)%amt, " TOTAL: ", (curr(k)%amt - lastNum)
                            nameTotal = nameTotal + (curr(k)%amt - lastNum)
                        endif
                    else
                        lastNum = curr(k)%amt + 1
                    endif

                    lastOper = curr(k)%oper
                endif
            enddo
            if (nameTotal > 0) subTotal = subTotal * nameTotal
        enddo

        total = total + subTotal
    endsubroutine

    recursive subroutine quicksort(arr, beg, end)
        type(Rule), intent(inout) :: arr(:)
        integer, intent(in) :: beg, end
        integer :: i, k
        type(Rule) :: pivot, temp

        if (beg < end) then
            pivot = arr(end)
            i = beg - 1

            do k = beg, end - 1
                if (arr(k)%amt <= pivot%amt) then  ! Sort by this field/property
                i = i + 1
                temp = arr(i)
                arr(i) = arr(k)
                arr(k) = temp
                end if
            end do

            temp = arr(i + 1)
            arr(i + 1) = arr(end)
            arr(end) = temp

            ! Recursively sort the sub-arrays
            call quicksort(arr, beg, i)
            call quicksort(arr, i + 2, end)
        end if
    end subroutine quicksort
end program aoc19b