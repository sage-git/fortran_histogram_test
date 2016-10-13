program test
  use histogram
  implicit none
  integer :: N,M,i
  type(hist_data) :: hist
  real(8), allocatable :: nd(:)
  real(8), allocatable :: random_values(:)
 
  write(*, *)"How many samples?"
  read(*,*) N
  write(*, *)"# of summation?"
  read(*,*) M

  allocate(nd(N))
  allocate(random_values(M))
 
  do i = 1, N
    call random_number(random_values)
    nd(i) = sum(random_values)/dble(M)
  end do
 
  call hist%load(nd, 25, 50)
  call hist%draw()
 
end program test
