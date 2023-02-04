module day1505_mod
  use parse_mod, only : string_t, read_strings
  implicit none

  character(len=*), parameter :: VOWELS = 'aeiou'
  character(len=2), parameter :: BLACKLISTED(4) = ['ab', 'cd', 'pq', 'xy']

contains

  subroutine day1505(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: i, ans1, ans2

    lines = read_strings(file)
    ans1 = 0
    ans2 = 0
    do i=1,size(lines)
      if (is_nice1(lines(i)%str)) ans1 = ans1 + 1
      if (is_nice2(lines(i)%str)) ans2 = ans2 + 1
      !print *, lines(i)%str, count_vowels(lines(i)%str), has_double(lines(i)%str), forbidden(lines(i)%str)
      !print *, lines(i)%str, is_nice2a(lines(i)%str), is_nice2b(lines(i)%str)
    end do
    print *, size(lines)
    print '("Answer 5/1 ",i0,l2)', ans1, ans1==238
    print '("Answer 5/2 ",i0,l2)', ans2, ans2==69
  end subroutine day1505


  pure logical function is_nice2(str) 
    character(len=*), intent(in) :: str
    is_nice2 = is_nice2a(str) .and. is_nice2b(str)
  end function is_nice2


  pure logical function is_nice2a(str)
    character(len=*), intent(in) :: str
!
! Nice if contains a pattern like "...XY...XY..." that does not overlap 
!
    integer :: i, j

    is_nice2a = .false.
    OUT: do i=1,len(str)-1
      associate(xy=>str(i:i+1))
        do j=i+2, len(str)-1
          if (str(j:j+1)==xy) then
            is_nice2a = .true.
            exit OUT
          end if
        end do
      end associate
    end do OUT
  end function is_nice2a


  pure logical function is_nice2b(str)
    character(len=*), intent(in) :: str
!
! Nice if contains a pattern like "...A?A..."
!
    integer :: i

    is_nice2b = .false.
    do i=1,len(str)-2
      if (str(i:i)==str(i+2:i+2)) then
        is_nice2b = .true.
        exit
      end if
    end do
  end function is_nice2b


  pure logical function is_nice1(str) 
    character(len=*), intent(in) :: str
    is_nice1 = count_vowels(str) >= 3 .and. has_double(str) .and. .not. forbidden(str)
  end function is_nice1


  pure logical function forbidden(str)
    character(len=*), intent(in) :: str

    integer :: i, j

    forbidden = .false.
    do i=1,len(str)-1
      if (any(str(i:i+1)==BLACKLISTED)) then
        forbidden = .true.
        exit
      end if
    end do
  end function forbidden


  pure logical function has_double(str)
    character(len=*), intent(in) :: str

    integer :: i

    has_double = .false.
    do i=1,len(str)-1
      if (str(i:i)==str(i+1:i+1)) then
        has_double = .true.
        exit
      end if
    end do
  end function has_double


  pure integer function count_vowels(str) result(n)
    character(len=*), intent(in) :: str

    integer :: i, j

    n = 0
    i = 1
    do
      j = scan(str(i:), VOWELS)
      if (j==0) exit ! no more vowwels
      n = n + 1 ! vowwel found
      i = j + i
      if (i>len(str)) exit ! behind the end of string
    end do
  end function count_vowels

end module day1505_mod