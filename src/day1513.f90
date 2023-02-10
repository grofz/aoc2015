module day1513_mod
  use parse_mod, only : string_t, read_strings, split
  use permutation_mod, only : permutation_generator_t
  use day1509_mod, only : add_name => add_city, &
  &  MAXNAMELEN => MAXCITYNAMELEN, find_name_index => find_city_index
  implicit none

contains

  subroutine day1513(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    character(len=MAXNAMELEN), allocatable :: names(:)
    character(len=MAXNAMELEN) :: read_names(2)
    type(permutation_generator_t) :: perm
    integer, allocatable :: mat(:,:), order(:)
    integer :: i, hapiness, j(2), ans1, ans2
    logical :: is_valid

    lines = read_strings(file)

    ! Make array of names
    allocate(names(0))
    do i=1, size(lines)
      call parse_line(lines(i)%str, read_names, hapiness)
      call add_name(read_names(1), names)
      call add_name(read_names(2), names)
    end do

    ! Construct the hapiness matrix
    allocate(mat(size(names), size(names)))
    mat = 0
    do i=1, size(lines)
      call parse_line(lines(i)%str, read_names, hapiness)
      j(1) = find_name_index(read_names(1), names)
      j(2) = find_name_index(read_names(2), names)
      mat(j(1),j(2)) = hapiness
    end do

    ! Test permutations and record the largest happiness
    allocate(order(size(names)))
    order = [(i, i=1, size(names))]
    call perm%init(order)
    ans1 = -huge(ans1) 
    ans2 = -huge(ans1) 
    do
      call perm%next(order, is_valid)
      if (.not. is_valid) exit
      hapiness = total_hapiness(order, mat, 1)
      if (hapiness > ans1) ans1 = hapiness
      hapiness = total_hapiness(order, mat, 2)
      if (hapiness > ans2) ans2 = hapiness
    end do

    do i=1,size(names)
      print '(a,3x,*(i3,1x))', names(i), mat(i,:)
    end do
    print '("Answer 13/1 ",i0,l2)', ans1, ans1==664 ! 676-591
    print '("Answer 13/2 ",i0,l2)', ans2, ans2==640 ! 676-591
  end subroutine day1513


  pure integer function total_hapiness(order, mat, imode) result(h)
    integer, intent(in) :: order(:), mat(:,:), imode

    integer :: i, ileft, iright, hap_left, hap_right

    h = 0
    do i=1, size(order)
      ileft = i - 1
      if (ileft==0) ileft = size(order)
      iright = i + 1
      if (iright==size(order)+1) iright = 1

      hap_left = mat(order(i), order(ileft))
      hap_right = mat(order(i), order(iright))

      if (i==1 .and. imode == 2) hap_left = 0
      if (i==size(order) .and. imode == 2) hap_right = 0

      h = h + hap_left + hap_right
    end do
  end function total_hapiness
  

  subroutine parse_line(str, cities, distance)
    character(len=*), intent(in) :: str
    character(len=MAXNAMELEN), intent(out) :: cities(2)
    integer, intent(out), optional :: distance

    type(string_t), allocatable :: words(:)

    call split(str, ' ', words)
    if (size(words) /= 11) error stop 'parse_line - format error (11 words expected)'
    cities(1) = words(1)%str
    cities(2) = words(11)%str
    associate (i => scan(cities(2), '.'))
      cities(2)(i:i) = ' ' 
    end associate
    if (present(distance)) then
      read(words(4)%str,*) distance
      select case(words(3)%str)
      case('gain')
      case('lose')
        distance = -distance
      case default
        error stop 'gains or looses expected'
      end select
    end if
  end subroutine parse_line

end module day1513_mod