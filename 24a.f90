program aoc24a
    implicit none

    type Ray
        real :: pos(3), vel(3)
    endtype

    character(len=75) :: line
    integer :: status, pos, i, k, total=0
    real :: slopeA, coeffA, slopeB, coeffB, crossX, crossY, tPos(3), tVel(3), limitMin, limitMax
    type(Ray), allocatable :: allRays(:)
    logical :: good

    allocate(allRays(0))
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        pos = index(line, " @ ")
        read (line(1:pos-1), *) tPos
        read (line(pos+3:), *) tVel
        ! print *, "TPOS:", tPos, "TVEL:", tVel

        allRays = [allRays, Ray(tPos, tVel)]
    end do
    ! print *, "ALLRAYS"
    ! do i=1, size(allRays)
    !     print *, "P:", allRays(i)%pos, "V:", allRays(i)%vel
    ! enddo

    ! limitMin = 7  ! For sample data
    ! limitMax = 27
    limitMin = 200000000000000.0  ! The .0 end to make a real literal, instead of int literal.
    limitMax = 400000000000000.0
    do i=1, size(allRays)
        do k=i+1, size(allRays)
            ! Use slopes/y-intercept algebra to solve. Formulas done mostly offline on paper, first.
            slopeA = allRays(i)%vel(2) / allRays(i)%vel(1)
            coeffA = allRays(i)%pos(2) - (allRays(i)%pos(1) * slopeA)

            slopeB = allRays(k)%vel(2) / allRays(k)%vel(1)
            coeffB = allRays(k)%pos(2) - (allRays(k)%pos(1) * slopeB)

            crossX = (coeffB - coeffA) / (slopeA - slopeB)  ! Solve for x: nx + a = mx + b
            crossY = (crossX * slopeA) + coeffA  ! Solve for y with one of the lines
            ! print *, i, " x ", k, " : ", crossX, " + ", crossY

            ! If within the interesting limited area
            if (crossX > limitMin .and. crossY > limitMin .and. crossX < limitMax .and. crossY < limitMax) then
                ! Test to be sure intersection was NOT in the PAST.
                good = .true.
                if (good .and. allRays(i)%vel(1) <= 0 .and. crossX > allRays(i)%pos(1)) good = .false.
                if (good .and. allRays(i)%vel(1) >= 0 .and. crossX < allRays(i)%pos(1)) good = .false.
                if (good .and. allRays(i)%vel(2) <= 0 .and. crossY > allRays(i)%pos(2)) good = .false.
                if (good .and. allRays(i)%vel(2) >= 0 .and. crossY < allRays(i)%pos(2)) good = .false.

                if (good .and. allRays(k)%vel(1) <= 0 .and. crossX > allRays(k)%pos(1)) good = .false.
                if (good .and. allRays(k)%vel(1) >= 0 .and. crossX < allRays(k)%pos(1)) good = .false.
                if (good .and. allRays(k)%vel(2) <= 0 .and. crossY > allRays(k)%pos(2)) good = .false.
                if (good .and. allRays(k)%vel(2) >= 0 .and. crossY < allRays(k)%pos(2)) good = .false.

                if (good) total = total + 1
            endif
        enddo
    enddo

    print *, "PART 1:", total
end program aoc24a