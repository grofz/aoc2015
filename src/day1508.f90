module day1508_mod
  use parse_mod, only : string_t, read_strings
  implicit none

contains

  subroutine day1508(file)
    character(*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: i, total_chars, active_chars, ans1
    integer :: new_chars, ans2

    lines = read_strings(file)
    total_chars = 0
    active_chars = 0
    new_chars = 0
    do i = 1, size(lines)
      call parse_string(lines(i)%str, total_chars, active_chars, new_chars)
    end do
    ans1 = total_chars - active_chars
    print '("Answer 8/1 ",i0,l2)', ans1, ans1==1342
    ans2 = new_chars - total_chars
    print '("Answer 8/2 ",i0,l2)', ans2, ans2==2074
  end subroutine day1508


  subroutine parse_string(str, total_chars, active_chars, new_chars)
    character(len=*), intent(in) :: str
    integer, intent(inout) :: total_chars, active_chars, new_chars

    integer :: i, n

    total_chars = total_chars + len(str)

    ! First and last char must be "
    n = len(str)
    if (str(1:1)/='"' .or. str(n:n)/='"') error stop 'parse - string not within apostrophes'

    ! Escape characters: \\, \", \xaa
    i = 2
    do
      if (str(i:i)=='\') then
        select case (str(i+1:i+1))
        case ('\','"')
          i = i + 2
        case ('x')
          i = i + 4
        case default
          error stop 'parse - unexpected escape character'
        end select
      else
        i = i + 1
      end if
      active_chars = active_chars + 1
      if (i>=n) exit
    end do

    ! Part 2
    do i = 1, n
      select case(str(i:i))
      case ('"', '\')
        new_chars = new_chars + 2
      case default
        new_chars = new_chars + 1
      end select
    end do
    new_chars = new_chars + 2 ! apostrophes
  end subroutine parse_string

end module day1508_mod