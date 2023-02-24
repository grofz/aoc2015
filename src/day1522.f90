module day1522_mod
  implicit none

  integer, parameter :: ID_PLAYER=1, ID_BOSS=2, NO_OF_EFFECTS=5
  integer, parameter :: MAX_MOVES = 100

  type character_t
    integer :: hp, dam=0, arm=0, mana=0
  end type character_t

  type effect_t
    integer :: cost
    integer :: timer_start
    integer :: timer=0
    integer :: damage=0
    integer :: heal=0
    integer :: mana=0
    integer :: arm=0
    character(len=3) :: str
  end type

  type state_t
    type(character_t) :: characters(2)
    type(effect_t) :: effects(NO_OF_EFFECTS)
    integer :: mana_spent=0 
    logical :: hard_difficulty=.false.
    integer :: nm=0, moves(MAX_MOVES)
  contains
    procedure, private :: state_less_than
    generic :: operator(<) => state_less_than
  end type
  
contains

  subroutine day1522(boss_hp, boss_dam)
    integer, intent(in) :: boss_hp, boss_dam

    integer, parameter :: PLAYER_HP = 50, PLAYER_MANA = 500
    type(state_t) :: init_state, order1, order2
    integer :: ans1, ans2

    init_state%characters(ID_PLAYER) = character_t(hp=PLAYER_HP, mana=PLAYER_MANA)
    init_state%characters(ID_BOSS) = character_t(hp=boss_hp, dam=boss_dam)
    init_state%effects = effects_init()

    call process_states(init_state, ans1, order1)
    call print_order(order1)
    print '("Answer 22/1 ",i0,l2)', ans1, ans1==1824

    init_state%hard_difficulty = .true.
    call process_states(init_state, ans2, order2)
    call print_order(order2)
    print '("Answer 22/2 ",i0,l2)', ans2, ans2==1937
  
  contains
    subroutine print_order(order)
      type(state_t) :: order
      integer :: i
      do i=1, order%nm
        write(*,'(a3,1x)',advance='no') order%effects(order%moves(i))%str
      end do
      write(*,'("mspend ",i0,"  hp ",i0,"  mana ",i0)') &
      & order%mana_spent, order%characters(ID_PLAYER)%hp, order%characters(ID_PLAYER)%mana
    end subroutine
  end subroutine day1522


  subroutine process_states(init_state, best, best_order)
    type(state_t), intent(in) :: init_state
    integer, intent(out) :: best
    type(state_t), intent(out) :: best_order

    type(state_t), allocatable :: states(:)
    type(state_t) :: current, new
    integer :: ns, i, ires
integer :: cnt, maxstates

    best = huge(best)
    call add_state(states, ns, init_state)
cnt = 0
maxstates = 0

    MAINLOOP: do
if (ns>maxstates) maxstates = ns
      if (ns==0) exit
!cnt = cnt+1
      current = states(1)
      states(1:ns-1) = states(2:ns) 
      ns = ns - 1

      ! Players turn
      if (current%hard_difficulty) then
        associate(hp=>current%characters(ID_PLAYER)%hp)
          hp = hp - 1
          if (hp == 0) cycle
        end associate
      end if

      call one_round(current%characters, current%effects, ID_PLAYER, ires)
      if (ires /= 0) then
        if (ires==ID_PLAYER .and. current%mana_spent < best) then
          best = current%mana_spent
          best_order = current
        end if
        cycle
      end if

      ! Loop over all player's choices
      do i=1, NO_OF_EFFECTS
        new = current

        ! Player can not choose an active spell 
        if (new%effects(i)%timer /= 0) cycle

        ! Not enough mana
        if (new%effects(i)%cost > new%characters(ID_PLAYER)%mana) cycle

        ! Activate chosen spell and make boss's turn
        new%effects(i)%timer = new%effects(i)%timer_start
        new%characters(ID_PLAYER)%mana = new%characters(ID_PLAYER)%mana - new%effects(i)%cost
        new%mana_spent = new%mana_spent + new%effects(i)%cost
        new%nm = new%nm+1
        new%moves(new%nm)=i
        call one_round(new%characters, new%effects, ID_BOSS, ires)

        if (ires==0) then
          ! Game continues / Drop if worse than already best solution
          if (new%mana_spent < best) then
            call add_state(states, ns, new)
cnt = cnt+1
          end if
        else if (ires==ID_PLAYER .and. new%mana_spent < best) then
          best = new%mana_spent
          best_order = new
        end if
      end do

    end do MAINLOOP
print *, 'counter =',cnt, maxstates
  end subroutine process_states


  subroutine add_state(states, ns, new_state)
    type(state_t), allocatable, intent(inout) :: states(:)
    integer, intent(inout) :: ns
    type(state_t), intent(in) :: new_state

    type(state_t), allocatable :: wrk(:)
    integer :: i

    ! Get more space if needed
    if (.not. allocated(states)) then
      allocate(states(100))
      ns = 0
    end if
    if (ns >= size(states)) then
print *, 'reallocation', ns
      allocate(wrk(2*size(states)))
      wrk(1:ns) = states(1:ns)
      call move_alloc(wrk, states)
    end if

    ! Insert new state at a correct position
    do i = ns, 1, -1
      if (new_state < states(i)) then
        states(i+1) = states(i)
        cycle
      end if
      exit
    end do
    states(i+1) = new_state
    ns = ns + 1
  end subroutine add_state


  logical function state_less_than(a, b) result(res)
    class(state_t), intent(in) :: a, b

! Makes DWS / BWS switch (DWS seems working the best)
!res = .false.
!return
    !
    ! Prioritize states with lowest boss's hp
    ! 
    if (a%characters(ID_BOSS)%hp < b%characters(ID_BOSS)%hp) then
      res = .true.
    else if (a%characters(ID_BOSS)%hp > b%characters(ID_BOSS)%hp) then
      res = .false.
    else
      if (a%mana_spent < b%mana_spent) then
        res = .true.
      else if (a%mana_spent > b%mana_spent) then
        res = .false.
      else
        res = .false.
      end if
    end if
  end function state_less_than


  subroutine one_round(this, effects, who, ires)
    type(character_t), intent(inout) :: this(2)
    type(effect_t), intent(inout) :: effects(:)
    integer, intent(in) :: who
    integer, intent(out) :: ires

    integer :: i, arm_bonus, damage


    ! Apply effects
    arm_bonus = 0
    do i=1, size(effects)
      if (effects(i)%timer == 0) cycle
      this(ID_BOSS)%hp = this(ID_BOSS)%hp - effects(i)%damage
      this(ID_PLAYER)%hp = this(ID_PLAYER)%hp + effects(i)%heal
      this(ID_PLAYER)%mana = this(ID_PLAYER)%mana + effects(i)%mana
      effects(i)%timer = effects(i)%timer - 1
      arm_bonus = arm_bonus + effects(i)%arm
    end do

    ires = 0
    if (this(ID_BOSS)%hp <= 0) then
      ires = ID_PLAYER
    else
      ! Boss attack
      if (who==ID_BOSS) then
        damage = max(1, this(who)%dam - (this(ID_PLAYER)%arm + arm_bonus))
        this(ID_PLAYER)%hp = this(ID_PLAYER)%hp - damage
      end if
      if (this(ID_PLAYER)%hp <= 0) ires = ID_BOSS
    end if
  end subroutine one_round


  function effects_init() result(effects)
    type(effect_t) :: effects(NO_OF_EFFECTS)

    ! Magick Missile
    effects(1) = effect_t(cost=53, timer_start=1, damage=4, str='MIS')
    ! Drain
    effects(2) = effect_t(cost=73, timer_start=1, damage=2, heal=2, str='DRA')
    ! Shield
    effects(3) = effect_t(cost=113, timer_start=6, arm=7, str='SHL')
    ! Poisson
    effects(4) = effect_t(cost=173, timer_start=6, damage=3, str='POI')
    ! Recharge
    effects(5) = effect_t(cost=229, timer_start=5, mana=101, str='REG')
  end function effects_init

end module day1522_mod
