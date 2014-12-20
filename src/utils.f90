module types
  integer, parameter:: dp=kind(0.d0), sp=kind(0.0)  ! double, simple precision
  integer, parameter:: MISSING_VALUE_INT = -99999
  real(dp), parameter:: MISSING_VALUE = -99999.d0, MISSING_VALUE_DP = -99999.d0
  integer, parameter :: clen = 256
end module types

module interp_mod

  use types, only: dp !, MISSING_VALUE
  implicit none
  private
  public :: get_interp_weights, get_interp_weights_stretched, locate

contains

  integer function locate(xx,x)
    ! Locate a value in a sorted array
    ! ==> adapted from lib_array but returns 0 or size+1 
    ! integer if an out-of-bound array is present

    implicit none
    real(dp), dimension(:), intent(in) :: xx
    real(dp), intent(in) :: x
    integer :: n,jl,jm,ju
    logical :: ascnd
    n=size(xx)
    ascnd = (xx(n) >= xx(1))

    ! border cases
    if (x == xx(1)) then
       locate = 1
       return
    else if (x == xx(n)) then
       locate = n
       return
    else if(ascnd.and. x > xx(n)) then
       locate = n+1
       return
    else if(ascnd.and. x < xx(1)) then
       locate = -1
       return
    else if(.not.ascnd.and. x > xx(n)) then
       locate = -1
       return
    else if(.not.ascnd.and. x < xx(1)) then
       locate = n+1
       return
    endif

    ! binary search in sorted array
    jl=0
    ju=n+1
    do
       if (ju-jl <= 1) exit
       jm=(ju+jl)/2
       if (ascnd .eqv. (x >= xx(jm))) then
          jl=jm
       else
          ju=jm
       end if
    end do

    ! matches element or is located left of it
    locate = jl

  end function locate

  ! ! regrid one array on another when the two axes only differ by a scaling factor
  ! subroutine regrid_stretched(x1, y1, x2, scal)
  !   real(dp), intent(in) :: x1(:), y1(:), x2(:)
  !   integer :: n1 = size(y1), n2 = size(y2)  ! ...and their sizes
  !
  ! end subroutine regrid_stretched

  ! compute weights of linear interp from one grid on another, general case
  subroutine get_interp_weights(x1, x2, l, w, bounds_error, linear_extrapolation, debug)
    real(dp), intent(in) :: x1(:), x2(:)
    integer, intent(out) :: l(size(x2))
    real(dp), intent(out) :: w(size(x2))       ! weight 
    logical, optional :: linear_extrapolation, bounds_error, debug
    logical :: linear_extrapolation_tmp = .false., bounds_error_tmp = .true., debug_tmp = .false.

    integer :: i, j, n1, n2, jl
    double precision :: dx1(size(x1)-1)

    if (present(bounds_error)) bounds_error_tmp = bounds_error
    if (present(linear_extrapolation)) linear_extrapolation_tmp = linear_extrapolation
    if (present(debug)) debug_tmp = debug

    n1=size(x1)
    n2=size(x2)

    if (.not. x1(n1)  > x1(1)) stop("get_interp_weight: assumes ascending array x1")
    if (.not. x2(n2)  > x2(1)) stop("get_interp_weight: assumes ascending array x2")
     
    do i = 1,n2
      jl = locate(x1, x2(i)) 
      if (jl == n1) then
        jl = n1 - 1
      else if (jl > n1) then
        if (bounds_error_tmp) then
          write(0,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') x2(i),x1(1),x1(n1)
          stop
        else if (linear_extrapolation_tmp) then
          jl = n1 - 1  ! do nothing
        else
          ! simply signal out-of-bound by setting w > 1
          l(i:) = n1 - 1
          w(i:) = 1 + 0.01d0
          exit 
        endif
      else if (jl < 1) then
        if (bounds_error_tmp) then
          write(0,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') x2(i),x1(1),x1(n1)
          stop
        end if
        jl = 1
      endif
      l(i) = jl
      w(i) = (x2(i)-x1(jl)) / (x1(jl+1) - x1(jl))
    enddo

    ! check
    if (debug_tmp) then
    ! if (.not.present(debug) .or. debug) then
      call check_interp_weights(x1, x2, l, l+1, w)
    endif

  end subroutine get_interp_weights

  ! compute interp weights between two grids when they differ from a scaling factor
  subroutine get_interp_weights_stretched(x1, x2, l, w, bounds_error, linear_extrapolation, debug)
    real(dp), intent(in) :: x1(:), x2(:)
    integer, intent(out) :: l(size(x2))  ! left and right indices
    real(dp), intent(out) :: w(size(x2))       ! weight 

    real(dp) :: scal
    integer :: i, j, n1, n2, jl, last_i2, i1,i2
    real(dp):: dx1(size(x1)-1)

    logical, optional :: bounds_error, linear_extrapolation, debug
    logical :: bounds_error_tmp = .true., linear_extrapolation_tmp = .false., debug_tmp = .false.

    if (present(bounds_error)) bounds_error_tmp = bounds_error
    if (present(linear_extrapolation)) linear_extrapolation_tmp = linear_extrapolation
    if (present(debug)) debug_tmp = debug

    n1=size(x1)
    n2=size(x2)

    ! if (n1 /= n2) then
    !   write(*,*) n1, n2
    !   stop("grid sizes differ")
    ! endif
    scal = (x2(2)-x2(1))/(x1(2)-x1(1))
    do i=1,n2
      if (x2(i) > x1(n1)) then
        if (bounds_error_tmp) then
          write(0,'("ERROR: Interpolation out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') x2(i),x1(1),x1(n1)
          stop
        else if (linear_extrapolation_tmp) then
          jl = n1 - 1  ! do nothing
        else
          ! simply signal out-of-bound by setting w > 1
          l(i:) = n1 - 1
          w(i:) = 1 + 0.01d0
          exit 
        end if
      endif
      jl = 1 + int((i-1)*scal)
      if (jl == n1) then
        jl = n1 - 1
      endif
      if (debug_tmp) then
        if (.not.(x1(jl)<=x2(i).and.x1(jl+1)>x2(i))) then
          print *, 'i,jl,scal, x1(jl-1:jl+1),x2(i)',i,jl,scal,x1(jl-1:jl+1), x2(i)
          stop("stretched grid jl determination failed")
        endif
      endif
      l(i) = jl
      w(i) = (x2(i)-x1(jl)) / (x1(jl+1) - x1(jl))
      if (w(i) > 1) then
        print *, 'jl',jl
        print *, 'scal',scal
        print *, 'i',i
        stop("check that out")
      endif
    enddo

    ! check
    if (debug_tmp) then
    ! if (.not.present(debug) .or. debug) then
      call check_interp_weights(x1, x2, l, l+1, w)
    endif

  end subroutine get_interp_weights_stretched

  subroutine check_interp_weights(x1, x2, l, r, w)
    real(dp), intent(in) :: x1(:), x2(:)
    integer, intent(in) :: l(size(x2)), r(size(x2))   ! left and right indices
    real(dp), intent(in) :: w(size(x2))   
    real(dp) :: scal    ! weight 

    integer :: i, j, n1, n2, jl

    integer i1, i2 ! debug
    logical :: check

    n1 = size(x1)
    n2 = size(x2)

    if (maxval(abs(x1(l)+w*(x1(r)-x1(l)) - x2), mask=(w>0.and.w<1)) > 0.1) then
      i = maxloc(abs(x1(l)+w*(x1(r)-x1(l)) - x2), mask=(w>0.and.w<1), dim=1)
      i1 = maxval([i-2,1])
      i2 = minval([i+2,size(w)])
      print *, 'first 5 x1',x1(:5)
      print *, 'first 5 x2',x2(:5)
      print *, ""
      print *, 'max mismatch:', i, 'x2=',x2(i)
      print *, ""
      print *, 'corresponds on x1, using locate:', locate(x1, x2(i))
      print *, '[l, r]', l(i),r(i)
      print *, '[x1(l), x1(r)]', x1(l(i)),x1(r(i))
      print *, 'w:', w(i)
      print *, 'x1(r)-x1(l):', x1(r(i))-x1(l(i))
      print *, 'x1(l)+w*(x1(r)-x1(l)):',x1(l(i))+w(i)*(x1(r(i))-x1(l(i)))
      print *, ""
      stop("problem when calculating the weights (stretched)")
    endif

    scal = (x2(2)-x2(1))/(x1(2)-x1(1))

    i = 1
    check = .false.
    do while (l(i) < size(x1).and.i<size(l))
      if (w(i) == 1) then
        check = .true.
        exit
      endif
      i = i + 1
    enddo
    if (check) then
      ! i = minloc(abs(w-1), dim=1)
      print *, 'scal', scal
      i1= max(1, i-1)
      i2 =min(size(w), i+1)
      print *, i1,':',i2
      print *, "x2",x1(i1:i2)
      print *, "x1",x1(l(i1):l(i1)+2)
      print *, "l",l(i1:i2)
      print *, "w",w(i1:i2)
      print *, x1(i1:i2) == x2(i1:i2)
      print *, x1(i1:i2) > x2(i1:i2)
      ! print *, "x1",x1(l(i1:i2))
      stop("check that out")
    endif

  end subroutine check_interp_weights

end module interp_mod

