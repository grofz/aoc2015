module day1519_mod
  use iso_fortran_env, only : I4B => int32
  use parse_mod, only : string_t, read_strings
  use tree_m, only : rbtr_t, DAT_KIND, tree_mold 
  use abstract_container, only : ERR_CONT_ISNOT, ERR_CONT_OK
  implicit none

  integer, parameter :: MAX_COMPOUND_LEN = 606, MAX_RULE_LEN = 10
  type compound_ptr
    character(len=MAX_COMPOUND_LEN), pointer :: ptr
  end type compound_ptr

  type rule_t
    character(len=MAX_RULE_LEN) :: src, dst
  end type

contains

  subroutine day1519(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(rule_t), allocatable :: rules(:), revrules(:)
    type(rbtr_t) :: tree
    type(compound_ptr) :: newdat
    character(len=MAX_COMPOUND_LEN) :: molecule
    integer :: nrule, i, j, ierr, ans1, counter, ans2
    integer(DAT_KIND), allocatable :: handle(:)

    ! Read rules and molecule from input
    lines = read_strings(file)
    nrule = size(lines)-2
    allocate(rules(nrule), revrules(nrule))
    if (lines(nrule+1)%str /= '') error stop 'day 19 - empty line expected'
    do i=1,nrule
      rules(i) = rule_parse(lines(i)%str)
      revrules(i)%src = rules(i)%dst
      revrules(i)%dst = rules(i)%src
    end do
    associate (init_str => lines(nrule+2)%str)
      if (len(init_str)>MAX_COMPOUND_LEN) error stop 'day 19 -increase MAX_COMPOUND_LEN'
      molecule = init_str
    end associate

    ! Store all possible replacement to the tree
    tree = rbtr_t(compare_nodes_fun)
    counter = 0

    do j=1,nrule
      i = 0
      do
        i = get_index_forward(molecule, rules(j), i)
        if (i==0) exit
        allocate(newdat%ptr) 
        counter = counter + 1
        newdat%ptr = trim(substitute_pattern(molecule, rules(j), i))
        call tree%Find(transfer(newdat,tree_mold), handle, ierr)
        select case(ierr)
        case(ERR_CONT_ISNOT) ! Not present in tree -> add
          call tree%Add(transfer(newdat,tree_mold))
          nullify(newdat%ptr)
        case(ERR_CONT_OK) ! Present in tree -> forget
          deallocate(newdat%ptr)
          counter = counter - 1
        case default
          error stop 'ierr wrong'
        end select
      end do
    end do
    ans1 = tree%Count()
    print '("Answer 19/1 ",i0,l2)', ans1, ans1==535

    ! Free memory
    call tree%Firstnode(handle, ierr)
    i = 0
    do
      if (ierr /= 0) exit
      newdat = transfer(tree%Read(handle), newdat)
      deallocate(newdat%ptr)
      counter = counter - 1
      i = i + 1
      call tree%Nextnode(handle, ierr)
    end do
    call tree%Removeall()
    if (counter /= 0) error stop 'day 19 - leeking memory check fail'

    ! Part 2
    call bws(molecule, revrules, ans2)
    print '("Answer 19/2 ",i0,l2)', ans2, ans2==212

  end subroutine day1519



  recursive subroutine bws(str, rules, nsteps)
    character(len=*), intent(in) :: str
    type(rule_t), intent(in) :: rules(:)
    integer, intent(out) :: nsteps
!
! Deep-wide search. We end as soon as any solution is found.
! TODO: Seems that only one solution exists, but can not proove.
!
    integer :: i, j, nsteps1
    character(len=MAX_COMPOUND_LEN) :: reduced
    
    if (str=='e') then
      nsteps = 0
      return
    end if

    nsteps = huge(nsteps)
    RLOOP: do j=1, size(rules)
      i = 0
      do
        i = get_index_forward(str, rules(j), i)
        if (i==0) exit
        reduced = trim(substitute_pattern(str, rules(j), i))
!print *, 'rule ',j,len_trim(reduced), trim(reduced)
        call bws(reduced, rules, nsteps1)
        nsteps = min(nsteps, nsteps1)
        ! End if ANY solution has been found
        if (nsteps1 /= huge(nsteps)) exit RLOOP
      end do
    end do RLOOP
    if (nsteps /= huge(nsteps)) then
      nsteps = nsteps + 1
    else
print *, 'no luck - this line is never run'
    end if
  end subroutine bws



  function get_index_forward(str, rule, i0) result(ind)
    character(len=*), intent(in) :: str
    type(rule_t), intent(in) :: rule
    integer, intent(in) :: i0
    integer :: ind

    integer :: i, lsrc

    lsrc = len_trim(rule%src)
    if (i0 /= 0) then
      if (str(i0:i0+lsrc-1) /= trim(rule%src)) error stop 'pattern - wrong ind'
      i = i0+lsrc
    else
      i = 1
    end if

    ind = index(str(i:), trim(rule%src))
    if (ind/=0) ind = i + ind - 1
  end function get_index_forward



  function substitute_pattern(str, rule, i0) result(new)
    character(len=*), intent(in) :: str
    type(rule_t), intent(in) :: rule
    integer, intent(in) :: i0
    character(len=MAX_COMPOUND_LEN) :: new

    integer :: lenold, lennew, lensrc, lendst

    lensrc = len_trim(rule%src)
    lendst = len_trim(rule%dst)
    lenold = len_trim(str)
    lennew = lenold + (lendst - lensrc)
    if (lennew > MAX_COMPOUND_LEN) error stop 'substitute_pattern - increase MAX_COMPOUND_LEN'
    if (i0==0) error stop 'substitute - invalid ind'
    if (str(i0:i0+lensrc-1) /= trim(rule%src)) error stop 'substitute - wrong ind'

    new = ''
    new(1:i0-1) = str(1:i0-1)
    new(i0+lendst:lennew) = str(i0+lensrc:lenold)
    new(i0:i0+lendst-1) = trim(rule%dst)
  end function substitute_pattern



  type(rule_t) function rule_parse(str) result(new)
    character(*), intent(in) :: str

    integer :: i1, i2

    i1 = index(str, ' => ')
    i2 = i1+3

    if (i1==0) error stop 'rule_parse - error format'
    if (i1-1 > MAX_RULE_LEN .or. len(str)-i2 > MAX_RULE_LEN) error stop 'rule_parse - increase MAX_RULE_LEN'
    new%src = str(:i1-1)
    new%dst = str(i2+1:)
  end function rule_parse



  pure function compare_nodes_fun(a,b) result(ires)
    integer(DAT_KIND), intent(in) :: a(:), b(:)
    integer :: ires

    type(compound_ptr) :: aobj, bobj

    aobj = transfer(a, aobj)
    bobj = transfer(b, bobj)

    if (aobj%ptr < bobj%ptr) then
      ires = 1
    elseif (aobj%ptr > bobj%ptr) then
      ires = -1
    else
      ires = 0
    end if
  end function compare_nodes_fun

end module day1519_mod
