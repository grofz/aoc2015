module day1517_mod
  use parse_mod, only : string_t, read_strings
  implicit none

contains

  subroutine day1517(file)
    character(*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer, allocatable :: x(:)
    logical, allocatable :: used(:)
    integer :: i, nrequired, ans1, mincon, ans2

    lines = read_strings(file)
    nrequired = 150
    !lines = read_strings('inp/17/test.txt')
    !nrequired = 25

    allocate(x(size(lines)), used(size(lines)))
    do i=1,size(lines)
      read(lines(i)%str,*) x(i)
    end do

    mincon = huge(mincon)
    ans1 = count_ways(used, x, nrequired, 1, mincon)
    print '("Answer 17/1 ",i0,l2)', ans1, ans1==654
    ans2 = count_ways(used, x, nrequired, 1, mincon)
    print '("Answer 17/2 ",i0,l2)', ans2, ans2==57
  end subroutine day1517


  recursive function count_ways(used, x, nreq, ind, mincon) result(cnt)
    logical, intent(inout) :: used(:)
    integer, intent(in) :: x(:), nreq, ind
    integer, intent(inout) :: mincon 
!
! If mincon is a large value on input, it is changed to the
! actual number of used buckets and this value is minimized.
! If mincon is a small value on input, it is not changed. Instead
! only solutions with the same number of used buckets is counted as solution
!
    integer :: cnt, n, mincon1, mincon2, i

    cnt = 0
    n = sum(x(1:ind-1), mask=used(1:ind-1))
    if (n > nreq) return
    if (ind==size(used)+1) then
      ! Path is complete - evaluate if it is valid
      if (n==nreq) then ! valid solution
        cnt = 1 
        if (mincon < size(used)) then
          if (count(used)/=mincon) then
            cnt = 0
          else
           !write(*,'(a)', advance='no') 'Solution: '
           !do i=1,size(x)
           !  if (used(i)) write(*,'(i3,1x)', advance='no'), x(i)
           !end do
           !write(*,*)
          end if
        else
          mincon = count(used)
        end if
      end if
    else
      ! Go into two possible directions (and recursively solve itself)
      ! one possible way: using the "ind"-th bucket
      mincon1 = mincon
      mincon2 = mincon
      used(ind) = .true.
      cnt = cnt + count_ways(used, x, nreq, ind+1, mincon1)
      ! second possible way: not using the bucket
      used(ind) = .false.
      cnt = cnt + count_ways(used, x, nreq, ind+1, mincon2)
      ! set output mincon value based on the mode 
      if (mincon < size(used)) then
        continue
      else
        mincon = min(mincon1, mincon2)
      end if
    end if
  end function count_ways

end module day1517_mod