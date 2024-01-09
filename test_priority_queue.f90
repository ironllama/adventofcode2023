program test_priority_queue
  use PriorityQueue
  implicit none

  integer, parameter :: n = 6
  type(t_priority_queue) :: pq
  integer :: x(n), i, k
  type(t_queue_item) :: payload

  call random_seed_clock()
  call randint(x,10*n)

  do i = 1, n
    call insert(pq,t_queue_item(-x(i), [(-x(i), k = 1, x(i))]))
    write(*,'("insert: ",i0," size: ",i0)') x(i), pq%num
  end do

  do while (pq%num > 0)
    payload = pull(pq)
    write(*,'("pull: ",i0," size: ",i0)') payload%p, pq%num
  end do

!   call randint(x,10*n)

!   do i = 1, n
!     call insert(pq,x(i))
!     write(*,'("insert: ",i0," size: ",i0)') x(i), pq%num
!   end do

!   write(*,'("peek: ",i0," size: ",i0)') peek(pq), pq%num

!   call clear(pq)
!   write(*,'("clear, size: ",i0)') pq%num

!   call randint(x,10*n)

!   do i = 1, n
!     call insert(pq,x(i))
!     write(*,'("insert: ",i0," size: ",i0)') x(i), pq%num
!   end do

!   write(*,'("peek: ",i0," size: ",i0)') peek(pq), pq%num

!   stop
contains

  subroutine random_seed_clock()
    implicit none
    integer :: nseed, clock
    integer, allocatable :: seed(:)

    call system_clock(clock)
    call random_seed(size=nseed)
    allocate(seed(nseed))
    seed = clock
    call random_seed(put=seed)
    deallocate(seed)
    return
  end subroutine random_seed_clock

  subroutine randint(x,maxint)
    implicit none
    integer, intent(out) :: x(:)
    integer, intent(in) :: maxint
    real(8) :: r(size(x))

    call random_number(r)
    x = int(maxint*r)
    return
  end subroutine randint

end program test_priority_queue
