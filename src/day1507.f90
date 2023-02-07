module day1507_mod
  use parse_mod, only : string_t, read_strings, split
  use iso_fortran_env, only : I16 => int16
  implicit none

  integer, parameter :: NAMEMAXLEN = 5
  integer, parameter :: OP_INPUT = 1, OP_AND = 2, OP_OR = 3, OP_NOT = 4, OP_RSH = 5, OP_LSH = 6
  character(len=6), parameter :: CH_OP(6) = ['->    ', 'AND   ','OR    ','NOT   ','RSHIFT', 'LSHIFT']

  type wire_t
    logical :: on = .false.
    character(len=NAMEMAXLEN) :: name
    integer(I16) :: val
  end type wire_t

  type gate_t
    integer :: op = 1
    character(len=NAMEMAXLEN) :: ione, itwo, out
  end type gate_t

contains

  subroutine day1507(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(gate_t), allocatable :: gates(:)
    type(wire_t), allocatable :: wires(:)
    type(wire_t) :: new_wire
    integer :: i, j, ind_a, ind_b
    logical :: is_all_evaluated, ison
    integer(I16) :: ans1, ans2

    ! Read gates from the input file
    lines = read_strings(file)
    allocate(gates(size(lines)))
    do i=1,size(lines)
      gates(i) = gate_parse(lines(i)%str)
    end do

    ! Read wire-names
    allocate(wires(0))
    do i=1,size(gates)
      associate (wname=>gates(i)%out)
        ! check that wire-name is unique
        do j=1, size(wires)
          if (wires(j)%name==wname) error stop 'wire-name is not unique'
        end do
        new_wire%name = wname
        wires = [wires, new_wire]
        if (wname=='a') ind_a = size(wires)
        if (wname=='b') ind_b = size(wires)
      end associate
    end do

    ! Evaluate the gates (Part 1)
    do
      is_all_evaluated = .true.
      do i=1,size(gates)
        call gate_evaluate(gates(i), wires, ison) 
        if (.not. ison) is_all_evaluated = .false.
      end do
      if (is_all_evaluated) exit
    end do
    ans1 = wires(ind_a)%val
    print '("Answer 7/1 ",i0,l2)', unsigned(ans1), unsigned(ans1)==956

    ! Part 2 - set "b" to "ans1" and reset remaining
    do i=1, size(wires)
      if (i==ind_b) then
        wires(i)%val = ans1
      else
        wires(i)%on = .false.
      end if
    end do

    ! Evaluate the gates (Part 2)
    do
      is_all_evaluated = .true.
      do i=1,size(gates)
        call gate_evaluate(gates(i), wires, ison) 
        if (.not. ison) is_all_evaluated = .false.
      end do
      if (is_all_evaluated) exit
    end do
    ans2 = wires(ind_a)%val
    print '("Answer 7/2 ",i0,l2)', unsigned(ans2), unsigned(ans2)==40149

  end subroutine day1507


  subroutine gate_evaluate(this, wires, ison)
    class(gate_t), intent(in) :: this
    type(wire_t), intent(inout) :: wires(:)
    logical, intent(out) :: ison

    integer(I16) :: val1, val2
    integer :: ind
    logical :: on1, on2

    ! Check if both inputs are on
    call wire_value(this%ione, wires, val1, on1)
    if (this%op==OP_INPUT .or. this%op==OP_NOT) then
      on2 = .true.
    else
      call wire_value(this%itwo, wires, val2, on2)
    end if
    ison = on1 .and. on2
    if (.not. ison) return

    ! Find the index for the output
    do ind=1,size(wires)
      if (wires(ind)%name == this%out) exit
    end do
    if (ind == size(wires)+1) error stop 'gate_evaluate - output wire not found'

    ! Evaluate the wire
    if (wires(ind)%on) return
    wires(ind)%on = .true.

    select case(this%op)
    case(OP_INPUT)
      wires(ind)%val = val1
    case(OP_AND)
      wires(ind)%val = iand(val1, val2)
    case(OP_OR)
      wires(ind)%val = ior(val1, val2)
    case(OP_NOT)
      wires(ind)%val = not(val1)
    case(OP_LSH)
      wires(ind)%val = ishft(val1, val2)
    case(OP_RSH)
      wires(ind)%val = ishft(val1, -val2)
    case default
      error stop 'gate_evaluate - uknonw op'
    end select
!print '("Op=",i1,1x,a,"=",i6," ",a,"=",i6," -> ",a,"=",i0)', &
!& this%op, adjustl(this%ione), val1, &
!& adjustl(this%itwo), val2, adjustl(this%out), wires(ind)%val
  end subroutine gate_evaluate


  subroutine wire_value(name, wires, val, ison)
    character(len=NAMEMAXLEN), intent(in) :: name
    type(wire_t), intent(in) :: wires(:)
    integer(i16), intent(out) :: val
    logical, intent(out) :: ison

    integer :: i

    if (len_trim(name)==0) error stop 'wire_value - empty string'
    associate(fch=>iachar(name(1:1)))
      if (fch>=iachar('0') .and. fch<=iachar('9')) then
        read(name,*) val
        ison = .true.
        return
      end if
    end associate

    do i=1, size(wires)
      if (wires(i)%name==name) exit
    end do
    if (i==size(wires)+1) error stop 'wire_value - wire name not found'
    if (wires(i)%on) then
      val = wires(i)%val
      ison = .true.
    else
      ison = .false.
    end if
  end subroutine wire_value


  type(gate_t) function gate_parse(str) result(new)
    character(len=*), intent(in) :: str

    type(string_t), allocatable :: toks(:)
    integer :: i, ione, itwo
    character(len=6) :: oper
    
    call split(str, ' ', toks)
    ione = 1
    itwo = 3
    oper = toks(2)%str
    i = findloc(CH_OP, oper, dim=1)
    if (i==0) then
      ione = 2
      itwo = 4
      oper = toks(1)%str
      i = findloc(CH_OP, oper, dim=1)
      if (i/=OP_NOT) error stop 'unknown operator'
    end if
    new%op = i
    new%ione = toks(ione)%str
    new%itwo = ''
    if (itwo < size(toks)) new%itwo = toks(itwo)%str

    if (toks(size(toks)-1)%str /= '->') error stop 'no output'
    new%out = toks(size(toks))%str

! print *, new%op, str
! print *, new%ione
! print *, new%itwo
! print *, new%out
  end function gate_parse


  integer function unsigned(val)
    integer(I16), intent(in) :: val
!
! 16bit signed to 16bit unsigned
! {0, 1, 2, h, -h-1, -3, -2, -1} => {0, 1, 2, h, h+1, h+2, h+3, h+4}
!
    if (val >= 0) then
      unsigned = val
    else
      unsigned = huge(val) + (huge(val) + 2 + val)
    end if
  end function unsigned

end module day1507_mod