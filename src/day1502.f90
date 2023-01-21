module day1502_mod
  use parse_mod, only : string_t, read_strings
  implicit none

  type packet_t
    integer :: l, w, h
  contains
    procedure :: area => packet_area
    procedure :: ribbon => packet_ribbon
  end type

contains

  subroutine day1502(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(packet_t), allocatable :: packets(:)
    integer :: i, ans1, ans2

    lines = read_strings(file)
    allocate(packets(size(lines)))
    ans1 = 0
    ans2 = 0
    do i = 1, size(lines)
      packets(i) = packet_new(lines(i)%str)
      ans1 = ans1 + packets(i)%area()
      ans2 = ans2 + packets(i)%ribbon()
    end do
    print '("Answer 2/1 ",i0,l2)', ans1, ans1==1606483
    print '("Answer 2/2 ",i0,l2)', ans2, ans2==3842356
  end subroutine day1502


  type(packet_t) function packet_new(str) result(new)
    character(len=*), intent(in) :: str

    integer :: i1, i2

    i1 = scan(str,'x')
    i2 = scan(str,'x',back=.true.)
    if (i1==0 .or. i2==0) error stop 'parse error'
    read(str(:i1-1),*) new%l
    read(str(i1+1:i2-1),*) new%w
    read(str(i2+1:),*) new%h
  end function packet_new


  integer function packet_area(this) result(area)
    class(packet_t), intent(in) :: this

    integer :: x(3), smallest(2), i

    x = [this%l, this%w, this%h]
    area = 2*x(1)*x(2) + 2*x(1)*x(3) + 2*x(2)*x(3)
    do i=1,2
      smallest(i) = minval(x)
      x(minloc(x,1)) = huge(x)
    end do
    area = area + smallest(1)*smallest(2)
  end function packet_area


  integer function packet_ribbon(this) result(length)
    class(packet_t), intent(in) :: this

    integer :: x(3), smallest(2), i, volu

    x = [this%l, this%w, this%h]
    volu = x(1)*x(2)*x(3)
    do i=1,2
      smallest(i) = minval(x)
      x(minloc(x,1)) = huge(x)
    end do
    length = 2*smallest(1)+2*smallest(2)+volu
  end function packet_ribbon

end module day1502_mod