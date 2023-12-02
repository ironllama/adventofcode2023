program maxLineLen
    implicit none

    integer :: status, fileMax, lineMax
    integer, parameter :: predictedMax = 10000
    character(len=10000) :: line

    fileMax = 0
    lineMax = 0
    status = 0

    do while (status == 0)
        read (*, '(a)', iostat=status) line
        if (status == -1) exit

        lineMax = len(trim(line))
        if (lineMax > fileMax) fileMax = lineMax
        ! print *, "LINE:", lineMax, fileMax, trim(line)
    end do

    print *, "MAX: ", fileMax
    if (fileMax >= predictedMax) print *, "PREDICTED MAX MET. You may want to try again with a larger predictedMax."

end program maxLineLen