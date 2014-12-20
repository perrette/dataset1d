! Test a dataset structure
module dataset_mod

  use types, only: dp, clen, MISSING_VALUE
  use interp_mod, only: get_interp_weights_stretched, get_interp_weights, locate

  implicit none

  private
  public :: Dataset

  type metadata_type
    character(len=clen) :: units
    character(len=clen) :: long_name 
    character(len=clen) :: desc
  end type metadata_type

  type Dataset

    real(dp), dimension(:,:), POINTER :: values
    real(dp), dimension(:), POINTER :: index
    integer :: iindex = 1  ! default position of the index
    character(len=clen), dimension(:), allocatable :: names
    type(metadata_type), dimension(:), allocatable :: metas
    ! type(metadata_type), dimension(:), POINTER :: metas
    integer :: nlen, nvar

  contains

    procedure :: alloc, dealloc
    procedure :: iname
    procedure :: set_index
    procedure :: copy
    procedure :: loc
    procedure :: slice, compress, take
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
    if (allocated(self%metas)) deallocate(self%metas)
    allocate(self%values(self%nlen, self%nvar) )
    allocate(self%names(self%nvar) )
    allocate(self%metas(self%nvar) )
  end subroutine alloc

  subroutine dealloc(self)
    class(Dataset), intent(inout) :: self
    deallocate(self%values)
    deallocate(self%names)
    deallocate(self%metas)
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
    integer :: i, maxlines = 50, clenmax
    character(len=clen) :: nvarc, clenc
    ! determine the output stream (screen or file)
    if (present(io)) then
      io_tmp = io
    else
      io_tmp = 0 !print to screen
    endif

    ! string representation of the number of variables
    write(nvarc,*) self%nvar
    ! determine the size of each column
    clenmax = 0
    do i=1,self%nvar
      clenmax = max(clenmax, len(trim(self%names(i))))
    enddo
    write(clenc,*) max(clenmax, 11)

    ! header
    write(io_tmp, '(" 1D-Dataset of ",I2," variables (nlen=",I2,")")') self%nvar, self%nlen
    ! variable names (columns)
    write(io_tmp,'('//nvarc//'(A'//clenc//',", "))') (trim(self%names(i)), i=1,self%nvar)
    ! actual  values
    do i=1,min(self%nlen,maxlines)
      write(io_tmp, '(I3," ",'//nvarc//'ES'//clenc//'.4)') i, self%values(i,:)
    enddo
    ! append last line, for long arrays
    if (self%nlen > maxlines) then
      write(io_tmp, *) '...'
      write(io_tmp, '(I3," ",'//nvarc//'ES'//clenc//'.4)') self%nlen, self%values(self%nlen,:)
    endif
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

    ipos = 0
    do i=1,self%nvar
      if (trim(self%names(i)) == trim(name)) then
        ipos = i
        exit
      endif
    enddo

    if (raise_error_tmp.and.ipos==0) then
      ! write(*,*) self%keys()
      write(*,*) "Variables:",(trim(self%names(i)), i=1,self%nvar)
      write(*,*) "ERROR: variable not found in dataset:",trim(name)
      stop
    endif
  end function iname

  ! ===========================================
  ! functions that return a dataset
  ! ===========================================

  type(Dataset) function copy(self) result(ds)
    class(Dataset), intent(in) :: self
    call ds%alloc(self%nlen, self%nvar)
    ds%values = self%values  ! copy
    ds%names = self%names  ! copy
    ds%metas = self%metas  ! copy
    call ds%set_index(self%iindex)
  end function copy

  ! ===========================================
  ! slice the dataset along the index dimension
  ! ===========================================
  type(Dataset) function slice(self, start, stop_, step, axis) result(ds)
    class(Dataset), intent(in) :: self
    ! integer, intent(in) :: start, stop_
    integer :: ipos
    integer, optional :: start, step, stop_
    integer :: start_tmp, step_tmp, stop_tmp
    integer, optional :: axis
    integer :: axis_tmp
    axis_tmp = 1
    if (present(axis)) axis_tmp = axis
    if (axis_tmp > 2 .or. axis_tmp < 1) stop("ERROR: slice: axis must be 0 or 1")
    start_tmp = 1
    stop_tmp = self%nlen
    step_tmp = 1
    if (present(start)) start_tmp = start
    if (present(stop_)) stop_tmp = stop_
    if (present(step)) step_tmp = step

    ds%nvar = self%nvar
    if (axis_tmp == 1) then
      ds%values => self%values(start_tmp:stop_tmp:step_tmp, :)
    else
      ds%values => self%values(:, start_tmp:stop_tmp:step_tmp)
    endif
    ds%nlen = size(ds%values(:,1))
    ds%nvar = size(ds%values(1,:))
    allocate(ds%names(ds%nvar))
    allocate(ds%metas(ds%nvar))
    if (axis_tmp == 1) then
      ds%names = self%names
      ds%metas = self%metas
    else
      ds%names = self%names(start_tmp:stop_tmp:step_tmp)
      ds%metas = self%metas(start_tmp:stop_tmp:step_tmp)
    endif
    call ds%set_index(self%iindex)
  end function slice

  ! ===========================================
  ! extract dataset when a mask is true
  ! ===========================================
  type(Dataset) function compress(self, mask, axis) result(ds)
    class(Dataset), intent(in) :: self
    logical, intent(in) :: mask(:)
    integer :: i,j, nlen, nvar, n
    integer, optional :: axis
    integer :: axis_tmp
    axis_tmp = 1
    if (present(axis)) axis_tmp = axis
    if (axis_tmp > 2 .or. axis_tmp < 1) stop("ERROR: slice: axis must be 0 or 1")

    if (axis_tmp == 1) then
      if (size(mask) /= self%nlen) stop("ERROR: compress: mask must have same size as indexed array's dimension")
      call ds%alloc(count(mask), self%nvar)
      j = 0
      do i=1,size(mask)
        if (mask(i)) then
          j = j+1
          ds%values(j,:) = self%values(i,:)
        endif
      enddo
      ds%names = self%names
      ds%metas = self%metas
      call ds%set_index(self%iindex)
    else
      if (size(mask) /= self%nvar) stop("ERROR: compress: mask must have same size as indexed array's dimension")
      call ds%alloc(self%nlen, count(mask))
      j = 0
      do i=1,size(mask)
        if (mask(i)) then
          j = j+1
          ds%values(:,j) = self%values(:,i)
          ds%names(j) = self%names(i)
          ds%metas(j) = self%metas(i)
        endif
      enddo
      if (mask(self%iindex)) then
        call ds%set_index(self%iindex)
      else
        call ds%set_index(1)
      endif
    endif
  end function compress

  ! ===========================================
  ! extract a few indices from a dataset
  ! ===========================================
  type(Dataset) function take(self, indices, axis) result(ds)
    class(Dataset), intent(in) :: self
    integer, intent(in) :: indices(:)
    integer :: i,j,nlen
    integer, optional :: axis
    integer :: axis_tmp
    axis_tmp = 1
    if (present(axis)) axis_tmp = axis
    if (axis_tmp > 2 .or. axis_tmp < 1) stop("ERROR: slice: axis must be 0 or 1")
    nlen = self%nlen

    if (axis_tmp == 1) then
      call ds%alloc(size(indices), self%nvar)
      ds%values = self%values(indices, :)
      ds%names = self%names
      ds%metas = self%metas
      call ds%set_index(self%iindex)
    else
      call ds%alloc(self%nlen, size(indices))
      ds%values = self%values(:, indices)
      ds%names = self%names(indices)
      ds%metas = self%metas(indices)
      if (ds%iname(self%names(self%iindex), raise_error=.false.) > 0) then
        call ds%set_index(self%iindex)
      else
        call ds%set_index(1)
      endif
    endif
  end function take

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
    call ds%alloc(nlen, self%nvar)
    ds%names = self%names
    ds%metas = self%metas
    call ds%set_index(self%iindex)

    if (stretched_tmp) then
      call get_interp_weights_stretched(self%index, newaxis, l, w, bounds_error, linear_extrapolation, debug)
    else
      call get_interp_weights(self%index, newaxis, l, w, bounds_error, linear_extrapolation, debug)
    endif

    do i=1,self%nvar
      ds%values(:, i) = self%values(l,i) + w*(self%values(l+1,i)-self%values(l,i))
    enddo
    
    if (.not.linear_extrapolation_tmp) then
      forall(i=1:nlen, w(i)<0.or.w(i)>1)
        ds%values(i,:) = fill_value_tmp
      end forall
    endif

  ! end subroutine interp
  end function interp

  ! ===========================================
  ! set / get an array in the dataframe
  ! ===========================================
  function getitem(self, name, start, stop_, step) result(array1d)
    class(Dataset), intent(in) :: self
    character(len=*), intent(in) :: name
    real(dp), POINTER :: array1d(:)
    integer :: ipos
    integer, optional :: start, step, stop_
    integer :: start_tmp, step_tmp, stop_tmp
    start_tmp = 1
    stop_tmp = self%nlen
    step_tmp = 1
    if (present(start)) start_tmp = start
    if (present(stop_)) stop_tmp = stop_
    if (present(step)) step_tmp = step
    ipos = self%iname(trim(name))
    ! call realloc(array1d, self%nlen)
    array1d => self%values(start_tmp:stop_tmp:step_tmp, ipos)
  end function getitem

  subroutine setitem(self, name, array1d,start, stop_, step)
    class(Dataset), intent(inout) :: self
    character(len=*), intent(in) :: name
    real(dp) :: array1d(:)    ! an array
    integer :: ipos
    integer, optional :: start, step, stop_
    integer :: start_tmp, step_tmp, stop_tmp
    start_tmp = 1
    stop_tmp = self%nlen
    step_tmp = 1
    if (present(start)) start_tmp = start
    if (present(stop_)) stop_tmp = stop_
    if (present(step)) step_tmp = step
    ipos = self%iname(trim(name))
    self%values(start_tmp:stop_tmp:step_tmp,ipos) = array1d  ! copy
  end subroutine setitem


end module
