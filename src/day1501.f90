module day1501_mod
  use parse_mod, only : read_strings, string_t
  implicit none

contains

  subroutine day1501(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: ans1, i, ans2

    lines = read_strings(file)
    ans1 = 0
    ans2 = 0
    do i=1, len(lines(1)%str)
      select case(lines(1)%str(i:i))
      case('(')
        ans1 = ans1 + 1
      case(')')
        ans1 = ans1 - 1
      case default
        error stop 'day 1 - unexpected character'
      end select
      if (ans1==-1 .and. ans2==0) ans2 = i
    end do
    print '("Answer 1/1 ",i0,l2)', ans1, ans1==280
    print '("Answer 1/2 ",i0,l2)', ans2, ans2==1797
  end subroutine day1501

end module day1501_mod