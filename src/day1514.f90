module day1514_mod
  use parse_mod, only : read_strings, string_t, split
  implicit none

  integer, parameter :: MAX_LEN=15

  type reindeer_t
    character(len=MAX_LEN) :: name
    integer :: v
    integer :: t_fly, t_rest
    integer :: score=0
  contains
    procedure :: distance => reindeer_distance
  end type
  interface reindeer_t
    module procedure reindeer_new
  end interface

contains

  subroutine day1514(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(reindeer_t), allocatable :: reindeers(:)
    integer :: i, maxscore, maxdis, t
    integer, parameter :: TIME_RACE = 2503

    lines = read_strings(file)
    allocate(reindeers(size(lines)))
    maxdis = 0
    do i=1,size(reindeers)
      reindeers(i) = reindeer_t(lines(i)%str)
      maxdis = max(maxdis, reindeers(i)%distance(TIME_RACE))
    end do
    print '("Answer 14/1 ",i0,l2)', maxdis, maxdis==2660

    ! Part Two
    do t=1, TIME_RACE
      call add_score(reindeers, t)
    end do
    maxscore = 0
    do i=1,size(reindeers)
      maxscore = max(maxscore, reindeers(i)%score)
    end do
    print '("Answer 14/2 ",i0,l2)', maxscore, maxscore==1256
  end subroutine day1514


  integer function reindeer_distance(this, t) result(d)
    class(reindeer_t), intent(in) :: this
    integer, intent(in) :: t

    integer :: ncycles, tremains, tcycle

    tcycle = this%t_fly+this%t_rest
    ncycles = t / tcycle
    tremains = mod(t, tcycle)
    d = ncycles * this%v * this%t_fly
    d = d + min(tremains, this%t_fly) * this%v
  end function reindeer_distance


  type(reindeer_t) function reindeer_new(str) result(new)
    character(len=*), intent(in) :: str

    type(string_t), allocatable :: words(:)

    call split(str, ' ', words)
    if (size(words)/=15) error stop 'reindeer_new - 15 words expected'
    new%name = words(1)%str
    read(words(4)%str, *) new%v
    read(words(7)%str, *) new%t_fly
    read(words(14)%str, *) new%t_rest
  end function reindeer_new


  subroutine sort_reindeers(arr, t)
    type(reindeer_t), intent(inout) :: arr(:)
    integer, intent(in) :: t

    type(reindeer_t) :: tmp
    integer :: i, j

    ! Insertion sort
    do i=2, size(arr)
      tmp = arr(i)
      do j=i,2,-1
        if (tmp%distance(t) > arr(j-1)%distance(t)) then
          arr(j) = arr(j-1)
        else
          exit
        end if
      end do
      arr(j) = tmp
    end do
  end subroutine sort_reindeers


  subroutine add_score(reindeers, t)
    type(reindeer_t), intent(inout) :: reindeers(:)
    integer, intent(in) :: t

    integer :: i, maxdis

    call sort_reindeers(reindeers, t)
    maxdis = reindeers(1)%distance(t)
    do i=1,size(reindeers)
      if (reindeers(i)%distance(t)==maxdis) then
        reindeers(i)%score = reindeers(i)%score + 1
      else
        exit
      end if
    end do
  end subroutine add_score

end module day1514_mod