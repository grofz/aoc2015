module day1521_mod
  implicit none

  type state_t
    integer :: wid=0, aid=0
    logical :: rings(6) = .false.
  contains
    procedure :: getstats => state_getstats
  end type state_t
  
  type character_t
    integer :: hp, dam, arm
  end type character_t

  integer, parameter :: ID_PLAYER=1, ID_BOSS=2

contains

  subroutine day1521(boss_hp, boss_dam, boss_arm)
    integer, intent(in) :: boss_hp, boss_dam, boss_arm

    integer, parameter :: MAX_DAM = 8+3, MAX_ARM = 5+3
    integer, parameter :: PLAYER_HP = 100
    integer :: cost_p2(MAX_DAM, MAX_ARM), cost_p1(MAX_DAM, MAX_ARM)
    integer :: i, j, ans1, ans2
    type(character_t) :: characters(2)

    characters(ID_BOSS) = character_t(hp=boss_hp, dam=boss_dam, arm=boss_arm)
    cost_p1 = 0
    cost_p2 = 0
    do i=1, MAX_DAM
    do j=1, MAX_ARM
      characters(ID_PLAYER) = character_t(hp=PLAYER_HP, dam=i, arm=j)
      associate(outcome=>battle_result(characters))
        if (outcome==ID_PLAYER) cost_p1(i,j) = minimal_cost(i,j)
        if (outcome==ID_BOSS) cost_p2(i,j) = minimal_cost(i,j,.true.)
      end associate
    end do
    end do
    ans1 = minval(cost_p1, mask=cost_p1>0)
    ans2 = maxval(cost_p2)
    print '("Answer 21/1 ", i0,l2)', ans1, ans1==121
    print '("Answer 21/2 ", i0,l2)', ans2, ans2==201
  end subroutine day1521


  pure integer function battle_result(this) result(ires)
    class(character_t), intent(in) :: this(2)

    type(character_t) :: wrk(2)

    wrk = this
    do
      call one_round(wrk, ires)
      if (ires/=0) exit
    end do
  end function battle_result


  pure subroutine one_round(this, ires)
    class(character_t), intent(inout) :: this(2)
    integer, intent(out) :: ires

    integer :: iatt, idef, damage

    ires = 0
    do iatt=1,2
      idef = 3-iatt
      damage = max(this(iatt)%dam - this(idef)%arm, 1)
      this(idef)%hp = this(idef)%hp - damage
      if (this(idef)%hp <= 0) then
        ires = iatt
        exit
      end if
    end do
  end subroutine one_round


  integer function minimal_cost(req_dam, req_arm, maxx) result(mincost)
    integer, intent(in) :: req_dam, req_arm
    logical, intent(in), optional :: maxx ! if present and true then Part 2 mode

    type(state_t) :: state
    integer :: i, j, k, m, dam, arm, cost, maxcost
    logical :: maxx0

    maxx0 = .false. 
    if (present(maxx)) maxx0 = maxx

    !
    ! This is really brute-force method, but the number of combinations
    ! is small enough.
    ! 
    ! In Part 1 - return the minimum cost to buy equipment that
    ! provides the required statistics (or better)
    !
    ! In Part 2 - return the maximum cost to buy equipment that
    ! provides the required statistics (or worse) 
    !
    mincost = huge(cost)
    maxcost = -1
    do i=1,5
      state%wid = i
      do j=1,5
        state%aid = j
        do k = 0, 2**6 - 1
          do m = 0, 5
          state%rings(m+1) = btest(k, m)
          end do

          if (.not. state_isvalid(state)) cycle
          call state%getstats(dam, arm, cost)
          if (maxx0) then
            if (dam > req_dam .or. arm > req_arm) cycle
          else
            if (dam < req_dam .or. arm < req_arm) cycle
          end if
          if (cost<mincost) mincost = cost
          if (cost>maxcost) maxcost = cost
        end do
      end do
    end do
    ! switch return value for Part 2 mode
    if (maxx0) mincost = maxcost

  end function minimal_cost


  pure logical function state_isvalid(this) result(isvalid)
    class(state_t), intent(in) :: this
    isvalid = this%wid>0 .and. this%wid <= 5 .and. &
              this%aid>0 .and. this%aid <= 5 .and. &
              count(this%rings) <= 2
  end function state_isvalid


  pure subroutine state_getstats(this, dam, arm, cost)
    class(state_t), intent(in) :: this
    integer, intent(out) :: dam, arm, cost

    integer, parameter :: WDAMS(5) = [4, 5, 6, 7, 8]
    integer, parameter :: WCOST(5) = [8, 10, 25, 40, 74]
    integer, parameter :: AARMS(5) = [1, 2, 3, 4, 5]
    integer, parameter :: ACOST(5) = [13, 31, 53, 75, 102]
    integer, parameter :: RDAMS(6) = [1, 2, 3, 0, 0, 0]
    integer, parameter :: RARMS(6) = [0, 0, 0, 1, 2, 3]
    integer, parameter :: RCOST(6) = [25, 50, 100, 20, 40, 80]

    if (.not. state_isvalid(this)) error stop 'state_getstat - invalid state'
    dam = WDAMS(this%wid) + sum(RDAMS, mask=this%rings)
    arm = AARMS(this%aid) + sum(RARMS, mask=this%rings)
    cost = WCOST(this%wid) + ACOST(this%aid) + sum(RCOST, mask=this%rings)
  end subroutine state_getstats

end module day1521_mod