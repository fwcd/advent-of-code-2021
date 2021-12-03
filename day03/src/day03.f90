program day03
  implicit none
  
  integer :: ios
  integer :: n
  integer, parameter :: count = 1000
  integer, dimension(count) :: xs

  open(1, file='resources/input.txt')

  do n = 1, count
    read(1, '(B32)', iostat=ios) xs(n)
    if (ios /= 0) exit
  end do

  close(1)
end program day03
