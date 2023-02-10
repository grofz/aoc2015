module day1510_mod
  implicit none

contains

  subroutine day1510(str)
    character(len=*), intent(in) :: str

    character(len=:), allocatable :: wstr
    integer :: i
    integer, parameter :: NREP_PART1 = 40, NREP_PART2 = 50

    wstr = str
    do i = 1, NREP_PART1
      !print '(i2,1x,i0)', i-1, len(wstr)
      write(*,'(a)',advance='no') '.'
      call look_and_say(wstr)
    end do
    print *
    print '("Answer 10/1 ",i0,l2)', len(wstr), len(wstr)==329356

    do i = NREP_PART1+1, NREP_PART2
      !print '(i2,1x,i0)', i-1, len(wstr)
      write(*,'(a)',advance='no') '.'
      call look_and_say(wstr)
    end do
    print *
    print '("Answer 10/2 ",i0,l2)', len(wstr), len(wstr)==4666278
  end subroutine day1510


  subroutine look_and_say(str)
    character(len=:), intent(inout), allocatable :: str

    character(len=:), allocatable :: old
    character(len=1) :: ch1, ch2
    integer :: i, cd, cd0, cnt, z 

    ! Avoid reallocation: estimate sufficiently large length
    ! The lengrh increases with Conway constant 
    ! https://www.youtube.com/watch?v=ea7lJkEhytA
    call move_alloc(str, old)
    allocate(character(len=int(1.5*len(old))) :: str)
    z = 0

    i = 1
    cd0 = -1
    cnt = 0
    do
      read(old(i:i),*) cd
      if (cd0==-1) cd0 = cd
      if (cd==cd0) then
        ! The sequence of the same digit continues
        cnt = cnt + 1
      else
        ! The sequence has ended / new sequence began
        call end_sequence()
        cnt = 1
        cd0 = cd
      end if
      i = i + 1
      if (i>len(old)) exit
    end do
    call end_sequence()

    ! Trim the result
    str = str(1:z)
  contains
    subroutine end_sequence()
        if (cnt>3) error stop 'day 15-10: unexpected digits'
        write(ch1,'(i1)') cnt
        write(ch2,'(i1)') cd0
        z=z+2
        str(z-1:z) = ch1//ch2
    end subroutine 
  end subroutine look_and_say

end module day1510_mod