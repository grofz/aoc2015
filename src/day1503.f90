module day1503_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day1503

  type position_t
    integer :: x(2)
  end type position_t

  character(len=1), parameter :: DCHAR(4) = ['<','^','>','v']
  integer, parameter :: DIR(2,4) = reshape([-1,0, 0,1, 1,0, 0,-1], [2,4])

contains

  subroutine day1503(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: line(:)
    type(position_t), allocatable :: visited(:)
    type(position_t) :: santa, robot
    integer :: nvisited, i

    line = read_strings(file)
    if (size(line)/=1) error stop 'day1503 - single line expected'

    ! Part 1
    allocate(visited(10))
    nvisited = 0
    santa%x = 0
    call add_item(visited, nvisited, santa)

    do i=1, len(line(1)%str)
      santa = move(santa, line(1)%str(i:i))
      call add_item(visited, nvisited, santa)
    end do
    print '("Answer 3/1 ",i0,l2)', nvisited, nvisited==2592

    ! Part 2
    deallocate(visited)
    allocate(visited(10))
    nvisited = 0
    santa%x = 0
    robot%x = 0
    call add_item(visited, nvisited, santa)

    do i=1, len(line(1)%str), 2
      santa = move(santa, line(1)%str(i:i))
      call add_item(visited, nvisited, santa)
      if (i<len(line(1)%str)) then
        robot = move(robot, line(1)%str(i+1:i+1))
        call add_item(visited, nvisited, robot)
      end if
    end do

    print '("Answer 3/2 ",i0,l2)', nvisited, nvisited==2360
  end subroutine day1503


  type(position_t) function move(cur, ch)
    type(position_t), intent(in) :: cur
    character(len=1), intent(in) :: ch

    integer :: i

    i = findloc(DCHAR, ch, dim=1)
    if (i==0) error stop 'move - uknown direction'
    move%x = cur%x + DIR(:,i)
  end function move


  subroutine add_item(arr, n, item)
    type(position_t), allocatable, intent(inout) :: arr(:)
    integer, intent(inout) :: n
    type(position_t), intent(in) :: item

    integer :: loc
    type(position_t), allocatable :: extarr(:)

    ! Add item to arr only if item not already present
    loc = find_loc(arr(1:n), item)
    if (loc/=0) return

    ! Extend array if out of space
    if (size(arr)==n) then
      allocate(extarr(2*n))
      extarr(1:n) = arr(1:n)
      call move_alloc(extarr, arr)
    end if

    ! Add item at the end of array
    n = n + 1
    arr(n) = item
  end subroutine add_item


  function find_loc(arr, item) result(loc)
    type(position_t), intent(in) :: arr(:)
    type(position_t), intent(in) :: item
    integer :: loc

    integer :: i

    loc = 0
    do i=1,size(arr)
      if (all(item%x==arr(i)%x)) then
        loc = i
        exit
      end if
    end do
  end function find_loc

end module day1503_mod