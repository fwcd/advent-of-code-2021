program day03
  implicit none
  
  integer :: ios
  character(len=32) :: line

  open(1, file = 'resources/input.txt')

  do
    read(1, '(A)', iostat=ios) line
    if (ios /= 0) exit
    print *, line
  end do

  close(1)
end program day03
