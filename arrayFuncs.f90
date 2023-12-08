module ArrayFuncs
    implicit none
    interface
        pure integer function f_int(x)
            integer, intent(in) :: x
        end function f_int

        pure logical function f_int_logical(x)
            integer, intent(in) :: x
        end function f_int_logical

        pure integer function f2_int(x, y)
            integer, intent(in) :: x, y
        end function f2_int
    end interface
contains
    ! Source: https://medium.com/modern-fortran/map-filter-reduce-in-fortran-2018-e40b93668ed9
    pure function map(f, x)
        procedure(f_int) :: f ! Mapping function
        integer, intent(in) :: x(:) ! Input array
        integer :: map(size(x)), i
        map = [(f(x(i)), i = 1, size(x))]
    end function map

    pure function filter(f, x)
        procedure(f_int_logical) :: f ! An int -> logical function
        integer, intent(in) :: x(:) ! Input array
        integer, allocatable :: filter(:)
        integer :: i
        filter = pack(x, [(f(x(i)), i = 1, size(x))])
    end function filter

    pure recursive integer function reduce(f, x, start) result(res)
        procedure(f2_int) :: f
        integer, intent(in) :: x(:), start
        if (size(x) < 1) then
            res = start
        else
            res = reduce(f, x(2:), f(start, x(1)))
        end if
    end function reduce

    pure recursive integer function reduce_right(f, x, start) result(res)
        procedure(f2_int) :: f
        integer, intent(in) :: x(:), start
        if (size(x) < 1) then
            res = start
        else
            res = f(x(1), reduce_right(f, x(2:), start))
        end if
    end function reduce_right

    ! pure elemental real function square(x)
    !     real, intent(in) :: x
    !     square = x**2
    ! end function square

    ! pure elemental recursive integer function fibonacci(n) result(fib)
    !     integer, intent(in) :: n
    !     if (n == 0) then
    !         fib = 0
    !     else if (n == 1) then
    !         fib = 1
    !     else
    !         fib = fibonacci(n-1) + fibonacci(n-2)
    !     end if
    ! end function fibonacci
end module ArrayFuncs