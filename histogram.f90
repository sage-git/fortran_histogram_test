module histogram
  implicit none
  type :: hist_data
    integer :: h, w
    integer :: max_count, num_per_char
    integer, allocatable :: hist(:)
    real(8) :: xmin, xmax, dx
  contains
    procedure :: load => hist_load
    procedure :: draw => hist_draw
  end type hist_data

contains
  subroutine hist_load(self, x, height, width)
    class(hist_data), intent(inout) :: self
    real(8), intent(in) :: x(:)
    integer, intent(in) :: height, width
    integer :: i
    self%h = height
    self%w = width
    self%xmin = minval(x)
    self%xmax = maxval(x)
    self%dx = (self%xmax - self%xmin)/height
    self%hist = (/(count(((i - 1)*self%dx + self%xmin < x) .and. &
              &           (x <= i*self%dx + self%xmin)),i = 1, height)/)
    self%max_count = maxval(self%hist)
    self%num_per_char = max(1, self%max_count / width)
  end subroutine hist_load

  subroutine hist_draw(self)
    class(hist_data), intent(in) :: self
    real(8) :: low, high
    character(len=32) :: F
    integer :: i, nchar
    F = '(f5.3,a,f5.3,a,i8,a,a)'
    do i = 1, self%h
      low = (i - 1)*self%dx + self%xmin
      high = i*self%dx + self%xmin
      nchar = self%hist(i)/self%num_per_char
      write(*,F) low," - ",high,": ", self%hist(i),":",repeat("#", nchar)
    enddo
  end subroutine hist_draw
end module histogram

