module day1524_mod
  use parse_mod, only : read_strings, string_t
  use iso_fortran_env, only : I8B=>int64
  implicit none

  type state_t
    integer, allocatable :: inps(:), used(:)
    integer :: reqsum
    integer :: pos = 0
  contains
    procedure, private :: state_less_than
    generic :: operator(<) => state_less_than
    procedure :: evaluate => state_evaluate
    procedure :: quantum => state_quantum
    procedure :: items => state_items
    procedure :: print => state_print
  end type state_t
  interface state_t
    module procedure state_fromfile
  end interface

  integer, parameter :: GRP_NONE = 0

contains

  subroutine day1524(file)
    character(len=*), intent(in) :: file

    type(state_t) :: init, sol, sol2
    integer(I8B) :: ans1, ans2

    init = state_t(file)
!init = state_t('inp/24/test.txt')

    call find_balance(init, 1, sol)
    ans1 = sol%quantum(1)
    call find_balance(sol, 2, sol2)
    call find_balance(sol2, 3, sol)
    call sol%print(1)
    call sol%print(2)
    call sol%print(3)
    print '("Answer 24/1 ",i0,l2)', ans1, ans1==11846773891_I8B
    print *

    init%reqsum = sum(init%inps)/4   
    call find_balance(init, 1, sol)
    ans2 = sol%quantum(1)
    call find_balance(sol, 2, sol2)
    call find_balance(sol2, 3, sol)
    call find_balance(sol, 4, sol2)
    call sol2%print(1)
    call sol2%print(2)
    call sol2%print(3)
    call sol2%print(4)
    print '("Answer 24/2 ",i0,l2)', ans2, ans2==80393059_I8B

  end subroutine day1524


  subroutine find_balance(init, group, best)
    type(state_t), intent(in) :: init
    integer, intent(in) :: group
    type(state_t), intent(out) :: best

    type(state_t), allocatable :: states(:)
    type(state_t) :: current, new
    integer :: ns, i, pool(2)
    logical :: any_solution

    call add_state(states, ns, init)
    states(1)%pos = 0
    pool(1) = group
    pool(2) = 0
    any_solution = .false.

    MAINLOOP: do
      if (ns==0) exit MAINLOOP
      current = states(1)
      states(1:ns-1) = states(2:ns) 
      ns = ns - 1

      ! Find free item
      do
        current%pos = current%pos+1
        if (current%pos > size(current%used)) error stop 'out of moves'
        if (current%used(current%pos)==GRP_NONE) exit
      end do

      ! Loop: pick-up or not pick-up the item
      !do i=GRP_ONE, GRP_NONE, -1
      do i=1,2
        new = current
        !new%used(new%pos) = i
        new%used(new%pos) = pool(i)
        select case(new%evaluate(group))
        case(1)  ! solution found
          if (.not. any_solution) then
            best = new
            any_solution = .true.
!print *, 'First solution'
!call best%print(group)
          else if (new%items(group) < best%items(group)) then
            best = new
!print *, 'Improved solution'
!call best%print(group)
          else if (new%items(group) == best%items(group) .and. new%quantum(group) < best%quantum(group)) then
            best = new
!print *, 'Improved solution'
!call best%print(group)
          else
            continue
          end if
        case(-1) ! failed, drop this path
        case(0)  ! add to queue
          if (.not. any_solution) then
            call add_state(states, ns, new)
          else if (new%items(group)<=best%items(group)) then
            call add_state(states, ns, new)
          end if
        end select
      end do
    end do MAINLOOP

print '(*(i1,1x))', best%used
    best%pos = size(best%used)
    if (.not. any_solution) error stop 'find_balance - no solution found'
  end subroutine find_balance


  subroutine add_state(states, ns, new_state)
    type(state_t), allocatable, intent(inout) :: states(:)
    integer, intent(inout) :: ns
    type(state_t), intent(in) :: new_state

    type(state_t), allocatable :: wrk(:)
    integer :: i

    ! Get more space if needed
    if (.not. allocated(states)) then
      allocate(states(100))
      ns = 0
    end if
    if (ns >= size(states)) then
      allocate(wrk(2*size(states)))
      wrk(1:ns) = states(1:ns)
      call move_alloc(wrk, states)
    end if

    ! Insert new state at a correct position
    do i = ns, 1, -1
      if (new_state < states(i)) then
        states(i+1) = states(i)
        cycle
      end if
      exit
    end do
    states(i+1) = new_state
    ns = ns + 1
  end subroutine add_state


  logical function state_less_than(a, b) result(res)
    class(state_t), intent(in) :: a, b

    if (a%pos > b%pos) then
      res = .true.
    else
      res = .false.
    end if
  end function state_less_than


  integer function state_evaluate(this, group) result(ires)
    class(state_t), intent(in) :: this
    integer, intent(in) :: group
!
! Return: 1 - if correct sum of items in the group    
!        -1 - not valid and can no longer be corrected
!         0 - invalid, but there are still moves
!
    integer :: actsum
    logical :: is_end

    is_end = .not. count(this%used(this%pos+1:)==GRP_NONE) > 0
if (this%pos==size(this%used)) is_end = .true.
    actsum = 0
    actsum = sum(this%inps(1:this%pos), mask=this%used(1:this%pos)==group)
    if (actsum==this%reqsum) then
      ires = 1
    else if (actsum > this%reqsum .or. is_end) then
      ires = -1
    else
      ires = 0
    end if
  end function state_evaluate


  integer(I8B) function state_quantum(this, group) result(iq)
    class(state_t), intent(in) :: this
    integer, intent(in) :: group

    integer :: i

    iq = 1
    do i=1, this%pos
      if (this%used(i)==group) iq = iq * this%inps(i)
    end do
  end function state_quantum


  integer function state_items(this, group) result(n)
    class(state_t), intent(in) :: this
    integer, intent(in) :: group

    n = count(this%used(1:this%pos)==group)
  end function state_items


  subroutine state_print(this, group)
    class(state_t), intent(in) :: this
    integer, intent(in) :: group

    integer :: i

    if (this%evaluate(group) /= 1) then
      write(*,'(a,i0)', advance='no') 'invalid', this%evaluate(group)
    else
      write(*,'(i19,": ")', advance='no') this%quantum(group)
    end if

    do i=1, this%pos
      if (this%used(i) /= group) cycle
      write(*,'(i0,1x)',advance='no') this%inps(i)
    end do
    write(*,'(" (items=",i0,"  sum=",i0,")")') &
    &  this%items(group), &
    &  sum(this%inps(1:this%pos), mask=this%used(1:this%pos)==group)

  end subroutine state_print


  type(state_t) function state_fromfile(file) result(new)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: i

    lines = read_strings(file)
    associate(n=>size(lines))
      allocate(new%inps(n), new%used(n))
      new%used = GRP_NONE
      do i=1,n
        read(lines(n-i+1)%str, *) new%inps(i)
      end do
    end associate
    new%reqsum = sum(new%inps)/3
    if (mod(sum(new%inps),3)/=0) error stop 'state_fromfile - input cannot be solved'
    if (mod(sum(new%inps),4)/=0) error stop 'state_fromfile - input cannot be solved in part2'
  end function state_fromfile


end module day1524_mod