program main
  use day1501_mod, only : day1501
  use day1502_mod, only : day1502
  use day1503_mod, only : day1503
  use day1504_mod, only : day1504
  use day1505_mod, only : day1505
  use day1506_mod, only : day1506
  use day1507_mod, only : day1507
  implicit none

  goto 07
  01 call day1501('inp/01/input.txt')
  02 call day1502('inp/02/input.txt')
  03 call day1503('inp/03/input.txt')
  04 call day1504()
  05 call day1505('inp/05/input.txt')
  06 call day1506('inp/06/input.txt')
  07 call day1507('inp/07/input.txt')

end program main
