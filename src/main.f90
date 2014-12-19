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
