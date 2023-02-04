module day1504_mod
  use day1605_mod, only : md5
  implicit none

contains

  subroutine day1504()
    character(len=*), parameter :: SALT = 'ckczppom'
    integer :: i, ans1, ans2
    integer, parameter :: MAX_TRY = 100000000
    character(len=9) :: dig
    character(len=32) :: hash

    !print '(a)', md5('abcdef609043')
    ans1 = 0
    ans2 = 0
    do i=1,MAX_TRY
      write(dig,'(i0)') i
      hash = md5(SALT//trim(adjustl(dig))) 
      if (ans1==0 .and. hash(1:5)=='00000') then
        ans1 = i
      end if
      if (ans2==0 .and. hash(1:6)=='000000') then
        ans2 = i
      end if
      if (mod(i,100000)==0) print *, i
      if (ans1/=0 .and. ans2/=0) exit
    end do
    if (i==MAX_TRY+1) error stop 'day 4 - not found'
    print '("Answer 4/1 ",i0,l2)', ans1, ans1==117946
    print '("Answer 4/2 ",i0,l2)', ans2, ans2==3938038
  end subroutine day1504

end module day1504_mod