! Test a dataset structure
module types
  implicit none
  integer, parameter:: dp=kind(0.d0), sp=kind(0.0), clen=256  ! double, simple precision

endmodule types

module dataset_mod

  use types, only: dp, clen

  implicit none

  private
  public :: Dataset

  type Dataset

    real(dp), dimension(:,:), POINTER :: values
    real(dp), dimension(:), POINTER :: index
    integer :: ix = 1  ! default position of the index
    character(len=clen), dimension(:), allocatable :: names
    integer :: nlen, nvar

  contains

    procedure :: alloc
    procedure :: iname
    procedure :: setitem
    procedure :: getitem
    procedure :: set_index
    procedure :: repr

  end type

contains

  subroutine alloc(self, names, nlen) 
    class(Dataset), intent(inout) :: self
    integer, intent(in) :: nlen
    character(len=*), intent(in) :: names(:)
    integer :: i, n
    real(dp), TARGET :: values_internal(nlen, size(names))

    self%nvar = size(names)
    self%nlen = nlen
    allocate(self%names(self%nvar) )
    self%names = names
    self%values => values_internal  ! dynamically allocated and associated

    ! also associate index
    self%index => self%values(:,self%ix)
  end subroutine alloc

  subroutine set_index(self, index)
    class(Dataset), intent(inout) :: self
    character(len=*), intent(in) :: index
    self%ix = self%iname(index)
    self%index => self%values(:, self%ix)
  end subroutine set_index

  character(len=256) function repr(self)
    class(Dataset), intent(in) :: self
    integer :: i, n = 4
    ! write(repr,*) "dims:",self%dims,"vars:",self%keys(), "len:",self%len
    write(*, *) "Dataset of",self%nvar,"1-D variables (nlen=",self%nlen,")"
    ! write(*, '("Dataset of",I2,"1-D variables (nlen=",I2,")")') self%nvar, self%nlen
    ! write(*, "('Dataset of',I2,'1-D variables (nlen=',I2,')')") self%nvar, self%nlen
    ! write(*, "(Dataset of ,I3, variables)") size(self%vars)
    write(*,*) "variables:"
    do i=1,self%nvar
      if (self%nlen > n) then
        write(*,*) trim(self%names(i)), self%values(1:n,i),'...'
      else
        write(*,*) trim(self%names(i)), self%values(:,i)
      endif
    enddo
    repr = ''
    ! write(repr,*) "dims:",self%dims
  end function repr

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
      write(*,*) self%names
      write(*,*) "item not found:",trim(name)
      stop("item not found in dataset")
    endif
  end function

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

end module

program main

  use types, only: clen, dp
  use dataset_mod, only: Dataset
  character(len=clen) :: names(3)

  type, EXTENDS(Dataset) :: Glacier
    real(dp), dimension(:), POINTER :: x
    real(dp), dimension(:), POINTER :: y
  end type Glacier

  ! type(Dataset) :: ds
  type(Glacier) :: ds

  names(1) = "x"
  names(2) = "v1"
  names(3) = "v2"
  ! print *, names

  call ds%alloc(names, 5)
  ds%x => ds%values(:,1)
  ds%y => ds%values(:,2)
  call ds%setitem('x',[5.d0,123.d0,67.d0,5.d0,11.d0])
  call ds%setitem('v1',ds%x**2)
  ds%index(1) = -99
  call ds%set_index('x')

  write(*,*) ds%repr()
  write(*,*) "index:",ds%index
  write(*,*) "getitem(v1):",ds%getitem('v1')
  write(*,*) 'x', ds%x
  write(*,*) 'y', ds%y
  write(*,*) 'sum(x)', sum(ds%x)
  write(*,*) 'mysum(x)', mysum(ds%x)  ! pointer but still works with functions written for arrays

  contains

    real(dp) function mysum(x)
      real(dp), intent(in) :: x(:)
      mysum = sum(x)
    end function mysum

end program 
