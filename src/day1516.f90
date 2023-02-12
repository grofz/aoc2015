module day1516_mod
  use parse_mod, only : string_t, read_strings, split
  implicit none

  integer, parameter :: MAX_WORD_LEN = 12

  type dictitem_t
    character(len=MAX_WORD_LEN) :: attr
    integer :: val
  end type dictitem_t
  interface dictitem_t
    module procedure dictitem_new
  end interface

  type dict_t
    integer :: id
    type(dictitem_t), allocatable :: items(:)
  end type dict_t
  interface dict_t
    module procedure dict_new
  end interface

contains

  subroutine day1516(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(string_t), allocatable :: match(:)
    type(dictitem_t), allocatable :: profile(:)
    integer :: i, ans1, ans2

    ! Read profile of the suspect
    match = read_strings('inp/16/match.txt')
    allocate(profile(size(match)))
    do i=1, size(match)
      profile(i) = dictitem_t(match(i)%str)
    end do

    lines = read_strings(file)
    ans1 = exclude(1)
    ans2 = exclude(2)
    print '("Answer 16/1 ",i0,l2)', ans1, ans1==40
    print '("Answer 16/2 ",i0,l2)', ans2, ans2==241
  contains

    integer function exclude(mode) result(ans)
      integer, intent(in) :: mode
      type(dict_t) :: tmp
      integer :: j

      ans = 0
      do i=1,size(lines)
        tmp = dict_t(lines(i)%str)
        do j=1, size(profile)
          if (has_conflicting_item(tmp, profile(j), mode)) exit
        end do
        if (j==size(profile)+1) then
          print *, 'Suspecting: ',lines(tmp%id)%str
          ans = tmp%id
        end if
      end do
    end function exclude
  end subroutine day1516


  pure logical function has_conflicting_item(this, item, mode) result(has)
    class(dict_t), intent(in) :: this
    class(dictitem_t), intent(in) :: item
    integer, intent(in) :: mode

    integer :: i

    has = .false.
    do i=1, size(this%items)
      if (item%attr /= this%items(i)%attr) cycle
      select case(mode)
      case(1)
        if (item%val == this%items(i)%val) cycle
      case(2)
        select case(item%attr)
        case ('cats', 'trees') ! are greater than the value
          if (item%val < this%items(i)%val) cycle
        case ('pomeranians', 'goldfish') ! are less than the value
          if (item%val > this%items(i)%val) cycle
        case default
          if (item%val == this%items(i)%val) cycle
        end select
      case default
        error stop 'error mode'
      end select
      has = .true.
      exit
    end do
  end function has_conflicting_item


  type(dict_t) function dict_new(str) result(new)
    character(len=*), intent(in) :: str
!
! Parse from line of input file
!
    integer :: i, n
    type(string_t), allocatable :: chunks(:)

    i = scan(str, ':')
    if (i==0 .or. str(1:4)/='Sue ') error stop 'dict_new - error parsing line'
    read(str(5:i-1),*) new%id
    call split(str(i+1:), ',', chunks)
    n = size(chunks)
    allocate(new%items(n))
    do i=1,n
      new%items(i) = dictitem_t(chunks(i)%str)
    end do
  end function dict_new


  type(dictitem_t) function dictitem_new(str) result(new)
    character(len=*), intent(in) :: str
!
! Parse from string: " attr: 123,"
!
    integer :: i, j

    i = scan(str,':')
    if (i==0) error stop 'dictitem_new - attribute not recognized'
    j = scan(str,',')
    if (j==0) j = len(str)+1
    if (i-1>MAX_WORD_LEN) error stop 'increase MAX LEN'
    new%attr = adjustl(str(:i-1))
    read(str(i+1:j-1),*) new%val
  end function dictitem_new

end module day1516_mod