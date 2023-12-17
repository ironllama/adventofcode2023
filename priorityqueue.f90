! Based on: https://qiita.com/ue1221/items/b923bb702c6e1a2d3cb2
module PriorityQueue
  implicit none

  type t_queue_item
    integer :: p
    integer, allocatable :: val(:)
  end type t_queue_item

  type t_priority_queue
    integer :: num = 0
    type(t_queue_item), pointer :: heap(:) => null()
  end type t_priority_queue

contains

  subroutine insert(pq,item)
    implicit none
    type(t_priority_queue), intent(inout) :: pq
    type(t_queue_item), intent(in) :: item
    integer :: n, i
    type(t_queue_item) :: t
    type(t_queue_item), allocatable :: tmp(:)

    if (.not.associated(pq%heap)) allocate(pq%heap(1))
    if (pq%num == size(pq%heap)) then
      allocate(tmp(pq%num))
      tmp = pq%heap
      deallocate(pq%heap)
      allocate(pq%heap(2*pq%num))
      pq%heap(1:pq%num) = tmp
      deallocate(tmp)
    end if

    pq%num = pq%num+1
    pq%heap(pq%num) = item

    n = pq%num
    do while (n > 1)
      i = n/2
      if (pq%heap(n)%p < pq%heap(i)%p) then
        t = pq%heap(n)
        pq%heap(n) = pq%heap(i)
        pq%heap(i) = t
      end if
      n = i
    end do
    return
  end subroutine insert

  subroutine clear(pq)
    implicit none
    type(t_priority_queue), intent(inout) :: pq

    if (associated(pq%heap)) deallocate(pq%heap)
    pq%num = 0
    return
  end subroutine clear

  function pull(pq) result(item)
    implicit none
    type(t_priority_queue), intent(inout) :: pq
    integer :: n, i, j
    type(t_queue_item) :: item, tmp

    n = pq%num
    item = pq%heap(1)
    pq%heap(1) = pq%heap(n)
    pq%num = pq%num-1

    i = 1
    do while (2*i < n)
      j = 2*i
      if (j+1 < n .and. pq%heap(j+1)%p < pq%heap(j)%p) j = j+1
      if (pq%heap(j)%p < pq%heap(i)%p) then
        tmp = pq%heap(j)
        pq%heap(j) = pq%heap(i)
        pq%heap(i) = tmp
      end if
      i = j
    end do
    return
  end function pull

  function peek(pq) result(item)
    implicit none
    type(t_priority_queue), intent(inout) :: pq
    type(t_queue_item) :: item

    item = pq%heap(1)
    return
  end function peek

end module PriorityQueue