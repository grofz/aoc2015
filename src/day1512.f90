! Notes
! - numbers
! - strings "aaa"
! - objects { "...":xxx, }
! - arrays [ 1, 2, 3]
!
! [ opens an array, { opens a group of obejcts, " is string, : specifies property
! ] ends array, } ends group, , separated objects


module day1512_mod
  use parse_mod, only : string_t, read_strings
  implicit none

  integer, parameter :: TYPE_NUM=1, TYPE_STR=2, TYPE_OBJ=3, TYPE_ARR=4
  integer, parameter :: MAX_STRING_LEN=10
  type elem_t
    integer :: t
    integer :: ival=0
    character(len=MAX_STRING_LEN) :: sval=''
    character(len=MAX_STRING_LEN) :: prop=''
    type(elem_ptr), allocatable :: chlds(:)
  end type

  type elem_ptr
    type(elem_t), pointer :: ptr
  end type elem_ptr

contains

  recursive function scan_elem(str, ind, prop, level) result(new)
    character(len=*), intent(in) :: str
    integer, intent(inout) :: ind
    character(len=*), intent(in) :: prop
    integer, intent(in) :: level
    type(elem_t), pointer :: new

    character(len=MAX_STRING_LEN) :: objprop
    type(elem_ptr) :: obj

    if (len(prop)>MAX_STRING_LEN) error stop 'property is too long'
    allocate(new)
    new%prop = prop
    allocate(new%chlds(0))

    if (ind>len(str)) error stop 'read_elem - index behind the string'
    select case(str(ind:ind))
    case('{')
      new%t = TYPE_OBJ
      ind = ind+1
    case('[')
      new%t = TYPE_ARR
      ind = ind+1
    case('"')
      new%t = TYPE_STR
    case default
      if (.not. isdigit(str(ind:ind))) error stop 'read_elem - unexpected char'
      new%t = TYPE_NUM
    end select

    select case(new%t)
    case (TYPE_NUM)
      new%ival = scan_the_digit(str, ind)
!call skip()
!print '("Read number ",i0)',new%ival
    case (TYPE_STR)
      new%sval = scan_the_string(str, ind)
!call skip()
!print '(a)', 'Read string '//new%sval
    case (TYPE_ARR, TYPE_OBJ)
      do
        objprop = scan_property(str, ind)
        obj%ptr => scan_elem(str, ind, objprop, level+1)
        new%chlds = [new%chlds, obj]

        if (ind>len(str)) error stop 'read_elem - unexpected end of string'
        select case(str(ind:ind))
        case(']')
          if (new%t/=TYPE_ARR) error stop 'read_elem - unexpected ]'
          ind = ind + 1
          exit
        case('}')
          if (new%t/=TYPE_OBJ) error stop 'read_elem - unexpected }'
          ind = ind + 1
          exit
        case(',')
          ind = ind + 1
        case default
          print *, 'error:',ind, str(ind:ind)
          error stop 'read_elem - error termination'
        end select
      end do
!call skip()
!print '("Read group ",i0,1x,i0)',new%t, size(new%chlds)

    case default
      error stop 'read_elem - invalid type'
    end select

  contains

    subroutine skip
      integer :: j
      do j=1,level
        write(*,'(A)',advance='no') ' '
      end do
    end subroutine skip
  end function scan_elem


  recursive subroutine free_elem(this)
    type(elem_t), intent(inout), pointer :: this

    integer :: i
    if (.not. allocated(this%chlds)) error stop 'chlds not allocated'
    do i=1,size(this%chlds)
      call free_elem(this%chlds(i)%ptr)
    end do
    deallocate(this%chlds)
    deallocate(this)
  end subroutine free_elem


  subroutine day1512(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: line(:)
    integer :: i, num, ans1, ans2
    type(elem_t), pointer :: whole

    line = read_strings(file)
    if (size(line)/=1) error stop 'day1512 - just single line expected'

    ! Quick and dirty method to solve just Part 1
    i = 1
    ans1 = 0
    do
      if (isdigit(line(1)%str(i:i))) then
        num = scan_the_digit(line(1)%str, i)
        ans1 = ans1 + num
      else
        i = i + 1
      end if
      if (i>len(line(1)%str)) exit
    end do
    print '("JSON string length ",i0)', len(line(1)%str)
    print '("Answer 12/1 ",i0,l2)', ans1, ans1==156366

    ! Part 1 and 2 by properly reading the structure
    i = 1
    whole => scan_elem(line(1)%str, i, '', 0)
    print '("Reding ok ? ",l1)',i==len(line(1)%str)+1
    ans1 = sumnumbers(whole,1)
    ans2 = sumnumbers(whole,2)
    print '("Answer 12/1 ",i0,l2)', ans1, ans1==156366
    print '("Answer 12/2 ",i0,l2)', ans2, ans2==96852
    call free_elem(whole)

  end subroutine day1512


  recursive function sumnumbers(this, mode) result(thesum)
    type(elem_t), intent(in) :: this
    integer, intent(in) :: mode
    integer :: thesum

    integer :: i
    logical :: is_red_object

    select case(this%t)
    case(TYPE_STR)
      thesum = 0
    case(TYPE_NUM)
      thesum = this%ival
    case(TYPE_ARR, TYPE_OBJ)
      is_red_object = .false.
      thesum = 0
      if (.not. allocated(this%chlds)) error stop 'children array not allocated'
      do i=1, size(this%chlds)
        associate(obj=>this%chlds(i)%ptr)
          thesum = thesum + sumnumbers(obj, mode)
          if (obj%t == TYPE_STR) then
            if (obj%sval=="red") is_red_object = .true.
          end if
        end associate
      end do
      if (this%t /= TYPE_ARR .and. mode == 2 .and. is_red_object) thesum = 0
    case default
      error stop 'sumnumbers - uknown type'
    end select
  end function sumnumbers


  pure logical function isdigit(ch)
    character(len=1), intent(in) :: ch

    if (iachar(ch)>=iachar('0') .and. iachar(ch)<=iachar('9')) then
      isdigit = .true.
    elseif (ch=='-') then
      isdigit = .true.
    else
      isdigit = .false.
    end if
  end function isdigit


  function scan_the_digit(str, i) result(num)
    character(len=*), intent(in) :: str
    integer, intent(inout) :: i
    integer :: num

    integer :: i0

    if (.not. isdigit(str(i:i))) error stop 'scan_the_digit - not a digit'
    i0 = i
    do
      if (i>len(str)) exit
      if (.not. isdigit(str(i:i))) exit
      i = i + 1
    end do
    read(str(i0:i-1),*) num
  end function scan_the_digit


  function scan_the_string(str, i) result(strval)
    character(len=*), intent(in) :: str
    integer, intent(inout) :: i
    character(len=MAX_STRING_LEN) :: strval

    integer :: i0

    if (.not. str(i:i)=='"') error stop 'scan_the_string - not a string'
    i = i + 1
    i0 = i
    do
      if (i>len(str)) error stop 'scan_the_string - string not ended'
      if (str(i:i)=='"') exit
      i = i + 1
    end do
    if (i-i0>MAX_STRING_LEN) error stop 'scan_the_string - increase MAX_STRING_LEN'
    strval = str(i0:i-1)
    i = i + 1 ! "i" points behind closing quote
  end function scan_the_string


  function scan_property(str, i) result(strval)
    character(len=*), intent(in) :: str
    integer, intent(inout) :: i
    character(len=MAX_STRING_LEN) :: strval

    integer :: i0
    i0 = i
    strval = ''
    if (str(i:i)=='"') then
      strval = scan_the_string(str, i)
      if (i>len(str)) error stop 'scan_property - unexpected end'
      if (str(i:i)==':') then
        i = i + 1
      else
        ! Not a property
        strval = ''
        i = i0
      end if
    end if
!print *, 'scan_property: <'//strval//'>',i
  end function scan_property

end module day1512_mod