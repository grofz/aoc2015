program main
  use day1501_mod, only : day1501
  use day1502_mod, only : day1502
  use day1504_mod, only : day1504
  implicit none

  goto 04
  01 call day1501('inp/01/input.txt')
  02 call day1502('inp/02/input.txt')
  04 call day1504()

end program main
