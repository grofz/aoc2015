module day1509_mod
  use parse_mod, only : string_t, read_strings, split
  use permutation_mod, only : permutation_generator_t
  implicit none

  integer, parameter :: MAXCITYNAMELEN = 14

contains

  subroutine day1509(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    character(len=MAXCITYNAMELEN), allocatable :: cities(:)
    character(len=MAXCITYNAMELEN) :: read_cities(2)
    type(permutation_generator_t) :: perm
    integer, allocatable :: dmat(:,:), order(:)
    integer :: i, j(2), ans1, ans2, distance
    logical :: is_valid

    lines = read_strings(file)

    ! Make array of city names
    allocate(cities(0))
    do i=1, size(lines)
      call parse_line(lines(i)%str, read_cities)
      call add_city(read_cities(1), cities)
      call add_city(read_cities(2), cities)
    end do

    ! Construct the distance matrix between cities
    allocate(dmat(size(cities), size(cities)))
    dmat = 0
    do i=1, size(lines)
      call parse_line(lines(i)%str, read_cities, distance)
      j(1) = find_city_index(read_cities(1), cities)
      j(2) = find_city_index(read_cities(2), cities)
      dmat(j(1),j(2)) = distance
      dmat(j(2),j(1)) = distance
    end do

    ! Test permutations and record the shortest/longest route
    allocate(order(size(cities)))
    order = [(i, i=1, size(cities))]
    call perm%init(order)
    ans1 = huge(ans1) ! minimum distance
    ans2 = 0          ! maximum distance
    do
      call perm%next(order, is_valid)
      if (.not. is_valid) exit
      distance = route_distance(order, dmat)
      if (distance < ans1) ans1 = distance
      if (distance > ans2) ans2 = distance
    end do

    do i=1,size(dmat,1)
      print '(a,*(i3,1x))', cities(i), dmat(i,:)
    end do

    print '("Answer 9/1 ",i0,l2)', ans1, ans1==117
    print '("Answer 9/2 ",i0,l2)', ans2, ans2==909
  end subroutine day1509


  pure function route_distance(order, dmat) result(dis)
    integer, intent(in) :: order(:), dmat(:,:)
    integer :: dis

    integer :: istart, iend, i

    dis = 0
    do i = 1, size(order)-1
      istart = order(i)
      iend = order(i+1)
      dis = dis + dmat(istart, iend)
    end do
  end function route_distance


  subroutine parse_line(str, cities, distance)
    character(len=*), intent(in) :: str
    character(len=MAXCITYNAMELEN), intent(out) :: cities(2)
    integer, intent(out), optional :: distance

    type(string_t), allocatable :: words(:)

    call split(str, ' ', words)
    if (size(words) /= 5) error stop 'parse_line - format error (5 words expected)'
    if (words(2)%str/='to' .or. words(4)%str/='=') error stop 'parse_line - format error'
    cities(1) = words(1)%str
    cities(2) = words(3)%str
    if (present(distance)) read(words(5)%str,*) distance
  end subroutine parse_line


  pure function find_city_index(city, cities) result(ind)
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
  end function find_city_index


  pure subroutine add_city(newcity, cities)
    character(len=*), intent(in) :: newcity
    character(len=MAXCITYNAMELEN), intent(inout), allocatable :: cities(:)
!
! Add the city to the table if not already present.
!
    integer :: i
    character(len=MAXCITYNAMELEN), allocatable :: tmp(:)

    ! City will be added only if not already in the array
    i = find_city_index(newcity, cities)
    if (i /=0) return 

    allocate(tmp(size(cities)+1))
    tmp(1:size(cities))(:) = cities
    tmp(size(cities)+1) = newcity
    call move_alloc(tmp, cities)
  end subroutine add_city

end module day1509_mod