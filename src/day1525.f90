module day1525_mod
  use iso_fortran_env, only : I8B => int64
  implicit none

  integer :: default_int

  type generator_t
    integer(I8B) :: code
    integer :: sid=huge(default_int)
  contains
    procedure :: get
  end type

contains

  subroutine day1525(row, col)
    integer, intent(in) :: row, col

    type(generator_t) :: g

    call g%get(calc_sid(row,col))
    print '("Answer 25/1 ",i0,l2)', g%code, g%code==19980801 

  end subroutine day1525


  integer function calc_sid(row, col) result(sid)
    integer, intent(in) :: row, col

    integer :: i

    sid = 1
    do i = 1, row + col-1
      sid = sid + i-1
    end do
    sid = sid + col-1
  end function calc_sid


  pure subroutine reset(this)
    type(generator_t), intent(out) :: this

    this%sid = 1
    this%code = 20151125
  end subroutine reset


  pure subroutine get(this, sid_req)
    class(generator_t), intent(inout) :: this
    integer, intent(in) :: sid_req

    if (this%sid > sid_req) call reset(this)
    do
      if (this%sid >= sid_req) exit
      this%code = this%code * 252533
      this%code = mod(this%code, 33554393)
      this%sid = this%sid + 1
    end do
  end subroutine get


end module day1525_mod