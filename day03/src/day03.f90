program day03
  implicit none
  
  integer, parameter :: count = 1000
  integer, parameter :: width = 12
  integer, dimension(count) :: xs
  integer :: ios, i, n, sum, b, gamma_rate, epsilon_rate

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
    ! Count the number of 1s
    sum = 0
    do n = 1, count
      sum = sum + iand(ishft(xs(n), -i), 1)
    end do
    ! The most common bit is 1 if there are at least (count / 2) of them
    if (sum >= (count / 2)) then
      b = 1
    else
      b = 0
    end if
    ! Update out gamma rate with this bit
    gamma_rate = ior(gamma_rate, ishft(b, i))
  end do

  ! Compute epsilon rate, which is just the bitwise negation of the gamma rate
  epsilon_rate = ieor(gamma_rate, (ishft(1, width) - 1))

  print *, 'Part 1:', gamma_rate * epsilon_rate

end program day03
