module day1513_mod
  use parse_mod, only : string_t, read_strings, split
  use permutation_mod, only : permutation_generator_t
  implicit none

  integer, parameter :: MAXNAMELEN = 8 

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
    !lines = read_strings('inp/13/test.txt')

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

    ! Test permutations and record the shortest/longest route
    allocate(order(size(names)))
    order = [(i, i=1, size(names))]
    call perm%init(order)
    ans1 = -huge(ans1) ! maximum hapiness
!   ans2 = 0          ! maximum distance
    do
      call perm%next(order, is_valid)
      if (.not. is_valid) exit
      hapiness = total_hapiness(order, mat)
if (hapiness>500) print ('(*(i0,1x))'), hapiness, order
      if (hapiness > ans1) ans1 = hapiness
     !if (distance > ans2) ans2 = distance
    end do

do i=1,size(names)
  print '(a,3x,*(i3,1x))', names(i), mat(i,:)
end do
    print '("Answer 13/1 ",i0,l2)', ans1, ans1==1 ! 676-591
  end subroutine day1513


  pure integer function total_hapiness(order, mat) result(h)
    integer, intent(in) :: order(:), mat(:,:)

    integer :: i, ileft, iright

    h = 0
    do i=1, size(order)
      if (i==1) then
        ileft = order(size(order))
      else
        ileft = order(i-1)
      end if
      if (i==size(order)) then
        iright = order(1)
      else
        iright = order(i+1)
      end if
      !h = h + mat(ileft,i) + mat(iright,i)
      h = h + mat(i,ileft) + mat(i,iright)
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


  pure subroutine add_name(newcity, cities)
    character(len=*), intent(in) :: newcity
    character(len=MAXNAMELEN), intent(inout), allocatable :: cities(:)
!
! Add the city to the table if not already present.
!
    integer :: i
    character(len=MAXNAMELEN), allocatable :: tmp(:)

    ! City will be added only if not already in the array
    i = find_name_index(newcity, cities)
    if (i /=0) return 

    allocate(tmp(size(cities)+1))
    tmp(1:size(cities))(:) = cities
    tmp(size(cities)+1) = newcity
    call move_alloc(tmp, cities)
  end subroutine add_name


  pure function find_name_index(city, cities) result(ind)
    character(len=*), intent(in) :: city
    character(len=*), intent(in) :: cities(:)
    integer :: ind
!
! Return the position of the city in the array.
! If city not present, return "0".
!
    integer :: i

    do i=1, size(cities)
      if (city==cities(i)) exit
    end do
    ind = i
    if (i==size(cities)+1) ind = 0
  end function find_name_index

end module day1513_mod