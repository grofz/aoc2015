module day1523_mod
  use parse_mod, only : read_strings, string_t, split
  implicit none
  private
  public day1523

  integer, parameter :: NO_REGS=2
  integer, parameter :: OP_HLF=1, OP_TPL=2, OP_INC=3, OP_JMP=4, OP_JIE=5, OP_JIO=6
  character(len=3), parameter :: CHOP_TEXT(6)=['hlf', 'tpl', 'inc', 'jmp', 'jie', 'jio']

  type computer_t
    private
    integer :: reg(NO_REGS)=0
    integer :: ip=1
    type(instruction_t), allocatable :: prog(:)
  contains
    procedure :: run => computer_run
  end type

  type instruction_t
    integer :: op, reg=0, arg=0
  end type
  interface instruction_t
    module procedure instruction_fromstr
  end interface

contains

  subroutine day1523(file)
    character(len=*), intent(in) :: file

    type(computer_t) :: computer
    type(string_t), allocatable :: lines(:)
    integer :: i, ans1, ans2

    lines = read_strings(file)
    allocate(computer%prog(size(lines)))
    do i = 1, size(lines)
      computer%prog(i) = instruction_t(lines(i)%str)
    end do

    call computer%run(0, ans1)
    print '("Answer 23/1 ",i0,l2)', ans1, ans1==255

    call computer%run(1, ans2)
    print '("Answer 23/2 ",i0,l2)', ans2, ans2==334
  end subroutine day1523



  subroutine computer_run(this, reg_a, reg_b)
    class(computer_t), intent(inout) :: this
    integer, intent(in) :: reg_a
    integer, intent(out) :: reg_b

    this%ip = 1
    this%reg = 0
    this%reg(1) = reg_a
    do
!print *, 'ip / a / b ', this%ip, this%reg
      if (this%ip<1 .or. this%ip>size(this%prog)) exit
      call computer_onestep(this)
    end do
    reg_b = this%reg(2)
  end subroutine computer_run



  subroutine computer_onestep(this)
    class(computer_t), intent(inout) :: this

    integer :: jump
    
    if (this%ip<1 .or. this%ip>size(this%prog)) error stop 'one step - ip out of bounds'
    jump = 1
    associate (inst=>this%prog(this%ip))
      if (inst%op == OP_JMP) then
        jump = inst%arg
      else
        associate(reg=>this%reg(inst%reg))
          select case(inst%op)
          case(OP_HLF)
            if (mod(reg,2)/=0) print *, 'warning - HLF not even number ', reg, this%ip
            reg = reg / 2
          case(OP_TPL)
            reg = reg * 3
          case(OP_INC)
            reg = reg + 1
          case(OP_JIE)
            if (mod(reg,2)==0) jump = inst%arg
          case(OP_JIO)
            if (reg==1) jump = inst%arg
          case default
            error stop 'one step - branching error'
          end select
        end associate
      end if
    end associate
    this%ip = this%ip + jump
  end subroutine computer_onestep



  type(instruction_t) function instruction_fromstr(str) result(new)
    character(len=*), intent(in) :: str

    type(string_t), allocatable :: toks(:)

    call split(str, ' ', toks)
    new%op = findloc(CHOP_TEXT, toks(1)%str, dim=1)
    if (new%op==0) error stop 'instruction_fromstr - uknown instruction'

    select case(new%op)
    case(OP_HLF, OP_TPL, OP_INC) ! hlf r
      if (size(toks)/=2) error stop 'instruction_fromstr - syntax error 1'
      new%reg = get_register(toks(2)%str)
    case(OP_JMP) ! jmp offset
      if (size(toks)/=2) error stop 'instruction_fromstr - syntax error 2'
      new%arg = get_argument(toks(2)%str)
    case(OP_JIE, OP_JIO) ! jie r, offset
      if (size(toks)/=3) error stop 'instruction_fromstr - syntax error 3'
      new%reg = get_register(toks(2)%str)
      new%arg = get_argument(toks(3)%str)
    case default
      error stop 'instruction_fromstr - invalid branch'
    end select

  contains

    integer function get_register(locstr) result(ind)
      character(len=*), intent(in) :: locstr

      if (len(locstr)>2) error stop 'get_register - register too long'
      if (len(locstr)==2) then
        if (locstr(2:2)/=',') error stop 'get_register - syntax error' 
      end if
      select case(locstr(1:1))
      case('a')
        ind = 1
      case('b')
        ind = 2
      case default
        error stop 'get_register - uknown register'
      end select
    end function get_register

    integer function get_argument(locstr) result(arg)
      character(len=*), intent(in) :: locstr
      integer :: ios

      read(locstr, *, iostat=ios) arg
      if (ios/=0) error stop 'get_argument - reading error'
    end function get_argument

  end function instruction_fromstr


end module day1523_mod