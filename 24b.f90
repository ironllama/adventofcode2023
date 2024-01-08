program aoc24b
    implicit none

    type Ray
        real(kind=8) :: x, y, z, vx, vy, vz
    endtype

    character(len=75) :: line
    integer :: status, pos, i, j, k, imax
    real(kind=8) :: tx, ty, tz, tvx, tvy, tvz, temp, sum, f, maxi=0
    integer(kind=8) :: total
    type(Ray), allocatable :: h(:)
    real(kind=8), allocatable :: a(:,:), c(:), x(:)

    allocate(h(0))
    do
        read (*, '(a)', iostat=status) line
        if (is_iostat_end(status)) exit

        pos = index(line, " @ ")
        read (line(1:pos-1), *) tx, ty, tz
        read (line(pos+3:), *) tvx, tvy, tvz

        h = [h, Ray(tx, ty, tz, tvx, tvy, tvz)]
    end do
    ! print *, "h"
    ! do i=1, size(h)
    !     print *, h(i)
    ! enddo

    ! Per hail, it's a x = x + vx*t and y = y + vy*t and z = z + vz*t. For the rock, it's r = r + vr*t. So:
    ! x + vx*t = rx + vrx*t  OR  (rx - x) / (vx - vrx) = t
    ! y + vy*t = ry + vry*t  OR  (ry - y) / (vy - vry) = t
    ! z + vz*t = rz + vrz*t  OR  (rz - z) / (vz - vrz) = t

    ! The rest of this commont of setup is taken from: https://github.com/DeadlyRedCube/AdventOfCode/blob/main/2023/AOC2023/D24.h
    ! Which explains the algebra much better than I can. To translate:
    ! x = A0x, B0x, and C0x for three separate hailstones needed to triangulate the rock vector
    ! y, z, vx, vy, vz follow similarly.
    ! rx = Px, vrx = Qx
    ! ry and rz follow similiarly.
    !
    ! Starting from Line 83:
    !
    ! We can eliminiate t, u, and v and end up with 6 equations with 6 unknowns (Px, Py, Pz, Qx, Qy, Qz):
    ! (Px - A0x) / (Avx - Qx) = (Py - A0y) / (Avy - Qy) = (Pz - A0z) / (Avz - Qz)
    ! (Px - B0x) / (Bvx - Qx) = (Py - B0y) / (Bvy - Qy) = (Pz - B0z) / (Bvz - Qz)
    ! (Px - C0x) / (Cvx - Qx) = (Py - C0y) / (Cvy - Qy) = (Pz - C0z) / (Cvz - Qz)

    ! Rearranging the Px/Py pairing:

    ! Px * Avy - Px * Qy - A0x * Avy + A0x * Qy = Py * Avx - Py * Qx - A0y * Avx + A0y * Qx
    ! (Px * Qy - Py * Qx) = (Px * Avy - Py * Avx) + (A0y * Avx - A0x * Avy) + (A0x * Qy - A0y * Qx)   {Equation 1}
    ! (Px * Qy - Py * Qx) = (Px * Bvy - Py * Bvx) + (B0y * Bvx - B0x * Bvy) + (B0x * Qy - B0y * Qx)   {Equation 2}
    ! (Px * Qy - Py * Qx) = (Px * Cvy - Py * Cvx) + (C0y * Cvx - C0x * Cvy) + (C0x * Qy - C0y * Qx)   {Equation 3}
    !
    ! Note that this gets a common (Px * Qy - Py * Qx) on the left side of everything, and the right side of each is
    !  now just a linear equation.
    ! Do the same for the Pz/Px and Py/Pz pairints:
    !
    ! (Pz * Qx - Px * Qz) = (Pz * Avx - Px * Avz) + (A0x * Avz - A0z * Avx) + (A0z * Qx - A0x * Qz)   {Equation 4}
    ! (Pz * Qx - Px * Qz) = (Pz * Bvx - Px * Bvz) + (B0x * Bvz - B0z * Bvx) + (B0z * Qx - B0x * Qz)   {Equation 5}
    ! (Pz * Qx - Px * Qz) = (Pz * Cvx - Px * Cvz) + (C0x * Cvz - C0z * Cvx) + (C0z * Qx - C0x * Qz)   {Equation 6}
    !
    ! (Py * Qz - Pz * Qy) = (Py * Avz - Pz * Avy) + (A0z * Avy - A0y * Avz) + (A0y * Qz - A0z * Qy)   {Equation 7}
    ! (Py * Qz - Pz * Qy) = (Py * Bvz - Pz * Bvy) + (B0z * Bvy - B0y * Bvz) + (B0y * Qz - B0z * Qy)   {Equation 8}
    ! (Py * Qz - Pz * Qy) = (Py * Cvz - Pz * Cvy) + (C0z * Cvy - C0y * Cvz) + (C0y * Qz - C0z * Qy)   {Equation 9}
    !
    ! Setting equations 1 and 2 equal to each other (to cancel out the common (Px * Qy - Py * Qx) term) gives us:
    ! [Avy - Bvy]Px - [Avx - Bvx]Py - [A0y - B0y]Qx + [A0x - B0x]Qy = (B0y * Bvx - B0x * Bvy) - (A0y * Avx - A0x * Avy)
    !
    ! If we similarly pair equations [1, 3], [4, 5], [4, 6], [7, 8], and [7, 9] together and simplify we end up with
    !  a series of 6 straight-up linear equations, which can be more easily solved than the above nonsense.
    !
    ! [Avy - Bvy]Px - [Avx - Bvx]Py - [A0y - B0y]Qx + [A0x - B0x]Qy = (B0y * Bvx - B0x * Bvy) - (A0y * Avx - A0x * Avy)
    ! [Avy - Cvy]Px - [Avx - Cvx]Py - [A0y - C0y]Qx + [A0x - C0x]Qy = (C0y * Cvx - C0x * Cvy) - (A0y * Avx - A0x * Avy)
    ! [Avx - Bvx]Pz - [Avz - Bvz]Px - [A0x - B0x]Qz + [A0z - B0z]Qx = (B0x * Bvz - B0z * Bvx) - (A0x * Avz - A0z * Avx)
    ! [Avx - Cvx]Pz - [Avz - Cvz]Px - [A0x - C0x]Qz + [A0z - C0z]Qx = (C0x * Cvz - C0z * Cvx) - (A0x * Avz - A0z * Avx)
    ! [Avz - Bvz]Py - [Avy - Bvy]Pz - [A0z - B0z]Qy + [A0y - B0y]Qz = (B0z * Bvy - B0y * Bvz) - (A0z * Avy - A0y * Avz)
    ! [Avz - Cvz]Py - [Avy - Cvy]Pz - [A0z - C0z]Qy + [A0y - C0y]Qz = (C0z * Cvy - C0y * Cvz) - (A0z * Avy - A0y * Avz)
    !
    ! For this code, we'll write it in matrix/vector notation:

    allocate(a(6,6))
    a(1,:) = [h(1)%vy - h(2)%vy, h(2)%vx - h(1)%vx, dble(0.0), h(2)%y - h(1)%y, h(1)%x - h(2)%x, dble(0.0)]  ! lines 1 and 2 - x, y
    a(2,:) = [h(1)%vy - h(3)%vy, h(3)%vx - h(1)%vx, dble(0.0), h(3)%y - h(1)%y, h(1)%x - h(3)%x, dble(0.0)]  ! lines 1 and 3 - x, y
    a(3,:) = [h(1)%vz - h(2)%vz, dble(0.0), h(2)%vx - h(1)%vx, h(2)%z - h(1)%z, dble(0.0), h(1)%x - h(2)%x]  ! lines 1 and 2 - x, z
    a(4,:) = [h(1)%vz - h(3)%vz, dble(0.0), h(3)%vx - h(1)%vx, h(3)%z - h(1)%z, dble(0.0), h(1)%x - h(3)%x]  ! lines 1 and 3 - x, z
    a(5,:) = [dble(0.0), h(1)%vz - h(2)%vz, h(2)%vy - h(1)%vy, dble(0.0), h(2)%z - h(1)%z, h(1)%y - h(2)%y]  ! lines 1 and 2 - y, z
    a(6,:) = [dble(0.0), h(1)%vz - h(3)%vz, h(3)%vy - h(1)%vy, dble(0.0), h(3)%z - h(1)%z, h(1)%y - h(3)%y]  ! lines 1 and 3 - y, z
    ! print *, "a", size(a, 1)
    ! do i=1, size(a, 1)
    !     print *, a(i,1:)
    ! enddo

    allocate(c(6))
    c(1) = (h(2)%y * h(2)%vx - h(2)%x * h(2)%vy) - (h(1)%y * h(1)%vx - h(1)%x * h(1)%vy)  ! lines 1 and 2 - x, y
    c(2) = (h(3)%y * h(3)%vx - h(3)%x * h(3)%vy) - (h(1)%y * h(1)%vx - h(1)%x * h(1)%vy)  ! lines 1 and 3 - x, y
    c(3) = (h(2)%z * h(2)%vx - h(2)%x * h(2)%vz) - (h(1)%z * h(1)%vx - h(1)%x * h(1)%vz)  ! lines 1 and 2 - x, z
    c(4) = (h(3)%z * h(3)%vx - h(3)%x * h(3)%vz) - (h(1)%z * h(1)%vx - h(1)%x * h(1)%vz)  ! lines 1 and 3 - x, z
    c(5) = (h(2)%z * h(2)%vy - h(2)%y * h(2)%vz) - (h(1)%z * h(1)%vy - h(1)%y * h(1)%vz)  ! lines 1 and 2 - y, z
    c(6) = (h(3)%z * h(3)%vy - h(3)%y * h(3)%vz) - (h(1)%z * h(1)%vy - h(1)%y * h(1)%vz)  ! lines 1 and 3 - y, z
    ! print *, "c", size(c)
    ! do i=1, size(c)
    !     print *, int8(c(i))
    ! enddo

    ! Gaussian Elimination (See: https://en.wikipedia.org/wiki/Gaussian_elimination)
    do k=1, size(a, 1)-1
        ! Pivoting
        imax = 1
        maxi = 0.0
        do i=k, size(a, 1)
            temp = a(i,k)
            if (temp < 0) temp = -1 * temp
            if (temp > maxi) then
                maxi = temp
                imax = i;
            endif
        enddo
        ! print *, "IMAX:", imax
        ! if (call equals(maxi, dble(0.0))) print *, "DANGER, WILL ROBINSON! DANGER!"

        temp = c(k)
        c(k) = c(imax)
        c(imax) = temp

        do i=1, size(a, 1)
            temp = a(k,i)
            a(k,i) = a(imax,i)
            a(imax,i) = temp
        enddo

        ! Row reduction
        do i=k+1, size(c)
            f = a(i,k) / a(k,k)
            ! print *, "F:", f, a(i,k), a(k,k)
            do j=k, size(a, 1)
                a(i,j) = a(i,j) - a(k,j) * f
            enddo
            c(i) = c(i) - c(k) * f
        enddo
    enddo

    ! Back-substitution
    allocate(x(6))
    do k=size(a, 1), 1, -1
        x(k) = c(k)
        do i=k+1, size(a, 1)
            x(k) = x(k) - a(k,i) * x(i)
        enddo
        x(k) = x(k) / a(k,k)
    enddo
    ! print *, "X:", x

    total = nint(x(1), 8) + nint(x(2), 8) + nint(x(3), 8)
    print *, "PART 2:", total

contains
    logical function equals (x, y)
        real, intent(in) :: x, y
        equals = abs((x) - (y)) < 0.0000000001 * (abs(x) + abs(y))
    endfunction

end program aoc24b