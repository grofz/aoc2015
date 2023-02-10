module day1511_mod
  implicit none

  integer, parameter :: PASSLEN = 8

contains
  subroutine day1511(str)
    character(len=*), intent(in) :: str

    character(len=PASSLEN) :: pass
    integer :: i

    if (len(str)/=PASSLEN) error stop 'day1511 - password not correct length'
    pass = str
    do i=1,2
      do
        call next_sequence(pass)
        if (has_allowed_chars(pass) .and. has_pair(pass) .and. has_triplet(pass)) exit
      end do
      if (i==1) print '("Answer 11/1 ",a,l2)', pass, pass=='hepxxyzz'
      if (i==2) print '("Answer 11/2 ",a,l2)', pass, pass=='heqaabcc'
    end do
  end subroutine day1511


  subroutine next_sequence(str)
    character(len=*), intent(inout) :: str

    integer :: i
    logical :: carry_one

    carry_one = .false.
    do i=len(str),1,-1
      str(i:i) = achar(iachar(str(i:i))+1)
      if (iachar(str(i:i))-1 == iachar('z')) then
        str(i:i) = 'a'
        carry_one = .true.
      else
        carry_one = .false.
      end if
      if (.not. carry_one) exit
    end do
    if (carry_one) error stop 'next_sequence - password space exhausted'
  end subroutine next_sequence


  pure logical function has_triplet(str)
    character(len=*), intent(in) :: str
!
! increasing straight at least three letters
!
    integer :: i, nstraight

    nstraight = 1
    do i=2,len(str)
      if (iachar(str(i-1:i-1))+1 == iachar(str(i:i))) then
        nstraight = nstraight + 1
        if (nstraight >= 3) exit
      else
        nstraight = 1
      end if
    end do
    has_triplet = nstraight >= 3
  end function


  pure logical function has_allowed_chars(str)
    character(len=*), intent(in) :: str
!
! forbidden letters are 'i' 'o' 'l'
!
    integer :: i, j, k

    i = scan(str, 'i')
    j = scan(str, 'o')
    k = scan(str, 'l')
    has_allowed_chars = i==0 .and. j==0 .and. k==0
  end function has_allowed_chars


  pure logical function has_pair(str)
    character(len=*), intent(in) :: str
!
! two different, non-overlapping pairs of letters
!
    integer :: i, npair
    character(len=1) :: chpair

    npair = 0
    chpair = '*'
    do i=1,len(str)-1
      if (str(i:i)/=str(i+1:i+1)) cycle
      if (str(i:i)/=chpair) then
        npair = npair + 1
        chpair = str(i:i)
      end if
      if (npair > 1) exit
    end do
    has_pair = npair >= 2
  end function has_pair

end module day1511_mod