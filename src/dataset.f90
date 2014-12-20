! Test a dataset structure
module dataset_mod

  use types, only: dp, clen, MISSING_VALUE
  use interp_mod, only: get_interp_weights_stretched, get_interp_weights, locate

  implicit none

  private
  public :: Dataset

  type Dataset

    real(dp), dimension(:,:), POINTER :: values
    real(dp), dimension(:), POINTER :: index
    integer :: iindex = 1  ! default position of the index
    character(len=clen), dimension(:), allocatable :: names
    integer :: nlen, nvar

  contains

    procedure :: alloc, dealloc
    procedure :: iname
    procedure :: set_index
    procedure :: copy
    procedure :: loc
    procedure :: slice, compress
    procedure :: getitem, setitem
    procedure :: interp
    procedure :: print => ds_print

  end type

contains

  subroutine alloc(self, nlen, nvar) 
    class(Dataset), intent(inout) :: self
    integer, intent(in) :: nlen, nvar
    self%nvar = nvar
    self%nlen = nlen
    if (allocated(self%names)) deallocate(self%names)
    allocate(self%values(self%nlen, self%nvar) )
    allocate(self%names(self%nvar) )
  end subroutine alloc

  subroutine dealloc(self)
    class(Dataset), intent(inout) :: self
    deallocate(self%values)
    deallocate(self%names)
  end subroutine

  subroutine set_index(self, iname)
    class(Dataset), intent(inout) :: self
    ! character(len=*), intent(in) :: index
    integer, intent(in) :: iname
    self%iindex = iname
    self%index => self%values(:, self%iindex)
  end subroutine set_index

  integer function loc(self, val, lower_bound)
    class(Dataset), intent(inout) :: self
    real(dp) :: val
    logical, optional :: lower_bound
    logical :: lower_bound_tmp
    if (present(lower_bound)) then
      lower_bound_tmp = lower_bound
    else
      lower_bound_tmp = .false.
    endif
    loc = locate(self%index, val)
    if (lower_bound_tmp) then
      if (loc < 1 .or. loc > self%nlen) then
        write(0,'("ERROR: dataset%loc : out of bounds : ",ES11.4," in [",ES11.4,":",ES11.4,"]")') &
          val,self%index(1),self%index(self%nlen)
      else if (self%index(loc) /= val) then
        write(0,'("ERROR: dataset%loc : no match found : ",ES11.4," in [",ES11.4,":",ES11.4,"]") '//&
          NEW_LINE('A')//'==> set lower_bound=.true. to retrieve the lower bound of this interval') &
          val,self%index(loc),self%index(loc+1)
      endif
    endif
  end function loc

  subroutine ds_print(self, io)
    class(Dataset), intent(in) :: self
    integer, optional :: io
    integer :: io_tmp
    integer :: i, n = 4
    if (present(io)) then
      io_tmp = io
    else
      io_tmp = 0 !print to screen
    endif
    ! write(repr,*) "dims:",self%dims,"vars:",self%keys(), "len:",self%len
    write(io_tmp, *) "Dataset of",self%nvar,"1-D variables (nlen=",self%nlen,")"
    ! write(*, '("Dataset of",I2,"1-D variables (nlen=",I2,")")') self%nvar, self%nlen
    ! write(*, "('Dataset of',I2,'1-D variables (nlen=',I2,')')") self%nvar, self%nlen
    ! write(*, "(Dataset of ,I3, variables)") size(self%vars)
    write(io_tmp,*) "variables:"
    do i=1,self%nvar
      if (self%nlen > n) then
        write(io_tmp,*) trim(self%names(i)), self%values(1:n,i),'...'
      else
        write(io_tmp,*) trim(self%names(i)), self%values(:,i)
      endif
    enddo
  end subroutine ds_print

  integer function iname(self, name, raise_error) result(ipos)
    class(Dataset), intent(in) :: self
    character(len=*), intent(in) :: name
    logical, optional :: raise_error
    logical :: raise_error_tmp
    integer :: i, length

    if (present(raise_error)) then
      raise_error_tmp = raise_error
    else
      raise_error_tmp = .true.
    endif

    do i=1,self%nvar
      if (trim(self%names(i)) == trim(name)) then
        ipos = i
        exit
      endif
    enddo

    if (raise_error_tmp.and.ipos>self%nvar) then
      ! write(*,*) self%keys()
      write(*,*) "Variables:",(trim(self%names(i)), i=1,self%nvar)
      write(*,*) "ERROR: variable not found in dataset:",trim(name)
      stop
    endif
  end function iname

  ! set / get a variable in the dataframe
  function getitem(self, name) result(array1d)
    class(Dataset), intent(in) :: self
    character(len=*), intent(in) :: name
    real(dp), POINTER :: array1d(:)
    integer :: ipos
    ipos = self%iname(trim(name))
    ! call realloc(array1d, self%nlen)
    array1d => self%values(:, ipos)
  end function getitem

  subroutine setitem(self, name, array1d)
    class(Dataset), intent(inout) :: self
    character(len=*), intent(in) :: name
    real(dp) :: array1d(:)    ! an array
    integer :: ipos
    ipos = self%iname(trim(name))
    self%values(:,ipos) = array1d  ! copy
  end subroutine setitem

  ! ===========================================
  ! functions that return a dataset
  ! ===========================================

  type(Dataset) function copy(self) result(ds)
    class(Dataset), intent(in) :: self
    call ds%alloc(self%nlen, self%nvar)
    ds%values = self%values  ! copy
    ds%names = self%names  ! copy
    call ds%set_index(self%iindex)
  end function copy

  ! ===========================================
  ! slice the dataset along the index dimension
  ! ===========================================
  type(Dataset) function slice(self, start, stop_, step) result(ds)
    class(Dataset), intent(in) :: self
    ! integer, intent(in) :: start, stop_
    integer, optional :: start, step, stop_
    integer :: start_tmp, step_tmp, stop_tmp
    integer :: ipos
    start_tmp = 1
    stop_tmp = self%nlen
    step_tmp = 1
    if (present(start)) start_tmp = start
    if (present(stop_)) stop_tmp = stop_
    if (present(step)) step_tmp = step

    ds%nvar = self%nvar
    ds%values => self%values(start_tmp:stop_tmp:step_tmp, :)
    ds%nlen = size(ds%values(:,1))
    allocate(ds%names(ds%nvar))
    ds%names = self%names
    call ds%set_index(self%iindex)
  end function slice

  ! ===========================================
  ! extract dataset when a mask is true
  ! ===========================================
  type(Dataset) function compress(self, mask) result(ds)
    class(Dataset), intent(in) :: self
    logical, intent(in) :: mask(:)
    real(dp), dimension(count(mask), self%nvar), TARGET :: subarray
    integer :: i,j,nlen
    nlen = self%nlen
    if (size(mask) /= nlen) stop("ERROR: compress: mask must have same size as indexed array's first dimension")
    j = 0
    do i=1,nlen
      if (mask(i)) then
        j = j+1
        subarray(j,:) = self%values(i,:)
      endif
    enddo
    ds%nvar = self%nvar
    ds%values => subarray
    ds%nlen = size(ds%values(:,1))
    allocate(ds%names(ds%nvar))
    ds%names = self%names
    call ds%set_index(self%iindex)
  end function compress

  ! ===========================================
  ! subset of variables in a dataset
  ! ===========================================
  type(Dataset) function subset(self, inames) result(ds)
    class(Dataset), intent(in) :: self
    integer :: inames(:)
    call ds%alloc(self%nlen, size(inames))
    ds%values = self%values(:, inames)
    allocate(ds%names(ds%nvar))
    ds%names = self%names
    call ds%set_index(self%iindex)
  end function subset

  ! =====================================
  ! Interpolation
  ! =====================================

  type(Dataset) function interp(self, newaxis, bounds_error, fill_value, stretched, linear_extrapolation, debug) result(ds)
    class(Dataset), intent(in) :: self
    ! class(Dataset), intent(out) :: ds
    real(dp), intent(in) :: newaxis(:)

    real(dp), dimension(size(newaxis), self%nvar), TARGET :: values
    logical, optional :: stretched, bounds_error, linear_extrapolation, debug
    real(dp), optional :: fill_value
    integer :: l(size(newaxis))
    real(dp) :: w(size(newaxis))
    logical :: stretched_tmp, linear_extrapolation_tmp
    real(dp) ::  fill_value_tmp
    integer :: i, nlen

    if (present(stretched)) then
      stretched_tmp = stretched
    else
      stretched_tmp = .false.
    endif
    if (present(fill_value)) then
      fill_value_tmp = fill_value
    else
      fill_value_tmp = MISSING_VALUE
    endif
    if (present(linear_extrapolation)) then
      linear_extrapolation_tmp = linear_extrapolation
    else
      linear_extrapolation_tmp = .false.
    endif

    nlen = size(newaxis)

    if (stretched_tmp) then
      call get_interp_weights_stretched(self%index, newaxis, l, w, bounds_error, linear_extrapolation, debug)
    else
      call get_interp_weights(self%index, newaxis, l, w, bounds_error, linear_extrapolation, debug)
    endif

    do i=1,self%nvar
      values(:, i) = self%values(l,i) + w*(self%values(l+1,i)-self%values(l,i))
    enddo
    
    if (.not.linear_extrapolation_tmp) then
      forall(i=1:nlen, w(i)<0.or.w(i)>1)
        values(i,:) = fill_value_tmp
      end forall
    endif

    ds%values => values
    ds%nlen = nlen
    ds%nvar = self%nvar
    allocate(ds%names(ds%nvar))
    ds%names = self%names
    call ds%set_index(self%iindex)

  ! end subroutine interp
  end function interp

end module
