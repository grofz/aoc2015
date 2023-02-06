module day1506_mod
  use parse_mod, only : string_t, read_strings, split
  use iso_fortran_env, only : int64
  implicit none

  type inst_t
    integer :: x0(2), x1(2), op
  end type inst_t

  integer, parameter :: OP_ON=1, OP_OFF=2, OP_TOGGLE=3

contains

  subroutine day1506(file)
    character(len=*), intent(in) :: file

    integer :: i, ans1
    integer(int64) :: ans2
    type(inst_t) :: inst
    type(string_t), allocatable :: lines(:)
    logical :: arr(0:999,0:999)
    integer :: ar2(0:999,0:999)

    arr = .false.
    ar2 = 0

    lines = read_strings(file)
    do i=1, size(lines)
      call modify(arr, inst_read(lines(i)%str))
      call modify2(ar2, inst_read(lines(i)%str))
    end do
    ans1 = count(arr)
    ans2 = sum(ar2)
    print '("Answer 6/1 ",i0,l2)', ans1, ans1==543903
    print '("Answer 6/2 ",i0,l2)', ans2, ans2==14687245

  end subroutine day1506


  type(inst_t) function inst_read(str) result(new)
    character(len=*), intent(in) :: str

    type(string_t), allocatable :: toks(:)
    integer :: i

    call split(str, ' ', toks)

    select case(toks(1)%str)
    case('toggle')
      new%op = OP_TOGGLE
      i = 2
    case('turn')
      select case(toks(2)%str)
      case('off')
        new%op = OP_OFF
      case('on')
        new%op = OP_ON
      case default
        error stop 'inst_read - second token uknonwn'
      end select
      i = 3
    case default
      error stop 'inst_read - first token uknonwn'
    end select

    new%x0 = read_num(toks(i)%str)
    if (toks(i+1)%str /= 'through') error stop 'inst_read - through not present'
    new%x1 = read_num(toks(i+2)%str)
  end function inst_read


  function read_num(str) result(x)
    character(len=*), intent(in) :: str
    integer :: x(2)

    integer :: i

    i = scan(str, ',')
    if (i==0) error stop 'read_num - comma not found'
    read(str(:i-1),*) x(1)
    read(str(i+1:),*) x(2)
  end function read_num


  subroutine modify(arr, inst)
    logical, intent(inout) :: arr(0:,0:)
    type(inst_t), intent(in) :: inst

    select case(inst%op)
    case(OP_OFF)
      arr(inst%x0(1):inst%x1(1), inst%x0(2):inst%x1(2)) = .false.
    case(OP_ON)
      arr(inst%x0(1):inst%x1(1), inst%x0(2):inst%x1(2)) = .true.
    case(OP_TOGGLE)
      where (arr(inst%x0(1):inst%x1(1), inst%x0(2):inst%x1(2))) 
        arr(inst%x0(1):inst%x1(1), inst%x0(2):inst%x1(2)) = .false.
      elsewhere
        arr(inst%x0(1):inst%x1(1), inst%x0(2):inst%x1(2)) = .true.
      endwhere
    end select
  end subroutine modify


  subroutine modify2(arr, inst)
    integer, intent(inout) :: arr(0:,0:)
    type(inst_t), intent(in) :: inst

    associate(a=> arr(inst%x0(1):inst%x1(1), inst%x0(2):inst%x1(2)))
      select case(inst%op)
      case(OP_OFF)
        a = max(a-1, 0)
      case(OP_ON)
        a = a + 1
      case(OP_TOGGLE)
        a = a + 2
      end select
    end associate
  end subroutine modify2

end module day1506_mod