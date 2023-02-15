module day1515_mod
  use parse_mod, only : string_t, read_strings, split
  implicit none

  integer, parameter :: NCOMP=5, MAXAMOUNT=100

contains

  subroutine day1515(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer, allocatable :: attributes(:,:), x(:)
    integer :: i, ningredients, ans1, ans2
    integer, parameter :: CALS_PART2=500

    lines = read_strings(file)
    ningredients = size(lines)
    allocate(attributes(NCOMP,ningredients))
    do i=1, ningredients
      attributes(:,i) = parse(lines(i)%str)
    end do

    allocate(x(ningredients))
    ans1 = get_maxscore(x,1,attributes,-1)
    print '("Answer 15/1 ",i0,l2)', ans1, ans1==222870 
    ans2 = get_maxscore(x,1,attributes,CALS_PART2)
    print '("Answer 15/2 ",i0,l2)', ans2, ans2==117936
  end subroutine day1515


  recursive function get_maxscore(x, ind, attributes, calreq) result(maxscore)
    integer, intent(inout) :: x(:)
    integer, intent(in) :: ind, attributes(:,:), calreq
    integer :: maxscore

    integer :: nremains, i

    nremains = MAXAMOUNT - sum(x(1:ind-1))
    if (ind==size(x)) then
      x(ind) = nremains
      if (x(ind)<0) error stop 'get_maxscore - negative amount'
      if (sum(x)/=MAXAMOUNT) error stop 'get_maxscore - total amount wrong'
      maxscore = eval_score(mix(x, attributes), calreq)
    else
      maxscore = 0
      do i=0,nremains
        x(ind) = i
        maxscore = max(maxscore, get_maxscore(x,ind+1,attributes,calreq))
      end do
    endif
  end function get_maxscore


  function eval_score(score, calreq) result(totscore)
    integer, intent(in) :: score(:), calreq
    integer :: totscore

    integer :: i

    ! Multiply individual attributes (all but calories)
    totscore = 1
    do i = 1, size(score)-1
      totscore = totscore * score(i)
    end do
    if (calreq > 0) then
      ! In part 2: Calories must aggree with requirement
      if (score(size(score))/=calreq) totscore = 0
    end if
  end function eval_score


  function mix(x, attributes) result(score)
    integer, intent(in) :: x(:), attributes(:,:)
    integer :: score(size(attributes,1))

    integer :: i, j

    if (size(x)/=size(attributes,2)) error stop 'mix - x and attributes not compatible'
    do i = 1, size(attributes,1)
      score(i) = 0
      do j = 1, size(attributes,2)
        score(i) = score(i) + x(j) * attributes(i,j)
      end do
      ! negative score transforms to 0 (calories are ignored)
      if (i/=size(attributes,1)) score(i) = max(0, score(i))
    end do
  end function mix


  function parse(str) result(attributes)
    character(len=*), intent(in) :: str
    integer :: attributes(NCOMP)

    type(string_t), allocatable :: toks1(:), toks2(:)
    integer :: i
    character(len=200) :: dummy

    ! Line to parse is
    !   Sugar: capacity 3, durability 0, flavor 0, texture -3, calories 2
    ! Ignore name of ingredient and split attributes with ',' separator
    call split(str, ':', toks1)
    if (size(toks1)/=2) error stop 'parse - error number one'
    call split(toks1(2)%str, ',', toks2)
    if (size(toks2)/=NCOMP) error stop 'parse - error number two'

    ! Ignore attribute name, but read its value
    do i=1,NCOMP
      read(toks2(i)%str,*) dummy, attributes(i)
    end do
  end function parse

end module day1515_mod