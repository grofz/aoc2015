module day1512_mod
  use parse_mod, only : string_t, read_strings
  implicit none

contains

  subroutine day1512(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: line(:)
    integer :: i, num, ans1

    line = read_strings(file)
    if (size(line)/=1) error stop 'day1512 - just single line expected'

    i = 1
    ans1 = 0
    do
      if (isdigit(line(1)%str(i:i))) then
        num = scan_the_digit(line(1)%str, i)
        ans1 = ans1 + num
!print *, i, num
      else
        i = i + 1
      end if
      if (i>len(line(1)%str)) exit
    end do
    print *, len(line(1)%str)
    print '("Answer 12/1 ",i0,l2)', ans1, ans1==156366
  end subroutine day1512


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

end module day1512_mod