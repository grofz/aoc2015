program main
  use day1501_mod, only : day1501
  use day1502_mod, only : day1502
  use day1503_mod, only : day1503
  use day1504_mod, only : day1504
  use day1505_mod, only : day1505
  implicit none

  goto 05
  01 call day1501('inp/01/input.txt')
  02 call day1502('inp/02/input.txt')
  03 call day1503('inp/03/input.txt')
  04 call day1504()
  05 call day1505('inp/05/input.txt')

end program main
