module day1520_mod
  implicit none

  integer, parameter :: P_ONEELF = 10
  integer, parameter :: P2_ONEELF = 11, H2_VISITED = 50

contains

  subroutine day1520(p_limit)
    integer, intent(in) :: p_limit

    integer, allocatable :: p(:)
    integer :: elf_max, elf, i, ans1, ans2

    elf_max = p_limit / P_ONEELF
    allocate(p(elf_max))

    ! Part One
    p = 0
    do elf = 1, elf_max
      do i = elf, elf_max, elf
        p(i) = p(i) + elf*P_ONEELF
      end do
    end do
    ans1 = find_house()
    print '("Answer 20/1 ",i0,l2)', ans1, ans1==786240

    ! Part Two
    p = 0
    do elf = 1, elf_max
      do i = elf, min(elf*H2_VISITED, elf_max), elf
        p(i) = p(i) + elf*P2_ONEELF
      end do
    end do
    ans2 = find_house()
    print '("Answer 20/2 ",i0,l2)', ans2, ans2==831600

  contains
    integer function find_house() result(ans)
      integer :: i
      do i=1, size(p)
        if (p(i) >= p_limit) exit
      end do
      ans = i
    end function find_house

  end subroutine day1520
end module day1520_mod