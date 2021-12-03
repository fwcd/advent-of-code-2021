module day03_functions
  implicit none
  contains

  function most_common_bit(i, words) result(b)
    implicit none

    integer, intent(in) :: i
    integer, intent(in), dimension(:) :: words
    integer :: n, sum, count, b

    ! Count the number of 1s
    sum = 0
    count = size(words)
    do n = 1, count
      sum = sum + iand(ishft(words(n), -i), 1)
    end do

    ! The most common bit is 1 if there are at least (count / 2) of them
    if (2 * sum >= count) then
      b = 1
    else
      b = 0
    end if
  end function

  subroutine compute_rating(use_least, words, width, rating)
    integer, dimension(:), intent(in) :: words
    integer, dimension(:), allocatable :: remaining
    integer, intent(in) :: use_least, width
    integer :: i, n, b, count
    integer, intent(out) :: rating

    count = size(words)

    allocate(remaining(count))
    do n = 1, count
      remaining(n) = words(n)
    end do

    do i = width - 1, 0, -1
      b = ieor(most_common_bit(i, remaining), use_least)
      print '(I2, B2, 999B7)', i, b, remaining
      remaining = pack(remaining, iand(ishft(remaining, -i), 1) == b)
      print '(I2, B2, 999B7)', i, b, remaining
      print *
      if (size(remaining) <= 1) exit
    end do

    rating = remaining(1)
  end subroutine
end module

program day03
  use day03_functions
  implicit none
  
  integer, parameter :: count = 12
  integer, parameter :: width = 5
  integer, dimension(count) :: xs
  integer :: ios, i, b, n, gamma_rate, epsilon_rate, oxygen_rating, co2_rating

  ! Read inputs line-by-line into xs
  open(1, file='resources/input.txt')
  do n = 1, count
    read(1, '(B32)', iostat=ios) xs(n)
    if (ios /= 0) exit
  end do
  close(1)

  ! Compute gamma rate
  gamma_rate = 0
  do i = 0, width - 1
    ! Update out gamma rate with this bit
    b = most_common_bit(i, xs)
    gamma_rate = ior(gamma_rate, ishft(b, i))
  end do

  ! Compute epsilon rate, which is just the bitwise negation of the gamma rate
  epsilon_rate = ieor(gamma_rate, (ishft(1, width) - 1))

  print *, 'Part 1:', gamma_rate * epsilon_rate

  ! Compute oxygen rating and co2 rating
  call compute_rating(0, xs, width, oxygen_rating)
  call compute_rating(1, xs, width, co2_rating)

  ! FIXME: Find out why part 2 still doesn't compute correctly
  print *, 'Part 2 (wrong):', oxygen_rating * co2_rating
end program day03
