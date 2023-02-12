module day1518_mod
  use parse_mod, only : read_pattern
  implicit none

  type board_t
    character(len=1), allocatable :: bb(:,:)
    integer :: time = 0
  contains
    procedure :: update => board_update
    procedure :: turnoncorners => board_turnoncorners
    procedure :: print => board_print
  end type
  character(len=1), parameter :: CH_ON='#', CH_OFF='.'

contains

  subroutine day1518(file)
    character(len=*), intent(in) :: file

    type(board_t) :: board
    integer, parameter :: NSTEPS=100
    integer :: ans1, ans2

    ! Part 1
    board%bb = read_pattern(file)
    call board%print()
    do
      call board%update(1)
      call board%print()
      if (board%time>=NSTEPS) exit
    end do
    ans1 = count(board%bb==CH_ON)

    ! Part 2
    board%bb = read_pattern(file)
    board%time = 0
    call board%turnoncorners()
    call board%print()
    do
      call board%update(2)
      call board%print()
      if (board%time>=NSTEPS) exit
    end do
    ans2 = count(board%bb==CH_ON)

    print '("Abswer 18/1 ",i0,l2)', ans1, ans1==1061
    print '("Abswer 18/2 ",i0,l2)', ans2, ans2==1006
  end subroutine day1518


  function count_on(bb) result(cnt)
    character(len=1), intent(in) :: bb(:,:)
    integer :: cnt(size(bb,1),size(bb,2))

    integer :: ii, jj, i, j, nx, ny
    nx = size(bb,1)
    ny = size(bb,2)
    do i=1,nx
    do j=1,ny
      cnt(i,j) = 0
      do ii=i-1,i+1
      do jj=j-1,j+1
        if (abs(i-ii)+abs(j-jj) < 1) cycle
        if (ii<1 .or. jj < 1 .or. ii>nx .or. jj>ny) cycle
        if (bb(ii,jj)==CH_ON) cnt(i,j)=cnt(i,j)+1
      end do
      end do
    end do
    end do
  end function count_on


  function new_state(old) result(new)
    character(len=1), intent(in) :: old(:,:)
    character(len=1) :: new(size(old,1),size(old,2))

    integer :: cnt(size(old,1),size(old,2))

    cnt = count_on(old)
    new = old
    where (old==CH_ON .and. (cnt/=2 .and. cnt/=3))
      new = CH_OFF
    else where (old==CH_OFF .and. cnt==3)
      new = CH_ON
    end where
  end function new_state


  subroutine board_update(this, mode)
    class(board_t), intent(inout) :: this
    integer, intent(in) :: mode
    this%bb = new_state(this%bb)
    this%time = this%time+1
    if (mode==2) call board_turnoncorners(this)
  end subroutine board_update

  subroutine board_turnoncorners(this)
    class(board_t), intent(inout) :: this
    this%bb(1,1) = CH_ON
    this%bb(1,size(this%bb,2)) = CH_ON
    this%bb(size(this%bb,1),1) = CH_ON
    this%bb(size(this%bb,1),size(this%bb,2)) = CH_ON
  end subroutine board_turnoncorners


  subroutine board_print(this)
    class(board_t), intent(in) :: this

    integer :: i
return
    write(*,'("Time = ",i0)') this%time
    do i=1, size(this%bb,1)
      write(*,'(*(a))') this%bb(i,:)
    end do
  end subroutine board_print

end module day1518_mod