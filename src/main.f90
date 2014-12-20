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
  type(Dataset) :: ds2

  print *, 'alloc'
  call ds%alloc(nlen=5, nvar=3)
  print *, 'names'
  ds%names(1) = "x"
  ds%names(2) = "v1"
  ds%names(3) = "v2"

  ds%x => ds%values(:,1)
  ds%y => ds%values(:,2)

  print *, 'setitem'
  call ds%setitem('x', [1.d0, 2.d0, 3.d0, 4.d0, 5.d0])
  call ds%setitem('v1',ds%x**2)
  call ds%setitem('v2',ds%x+0.d0)
  print *, 'set index'
  call ds%set_index(ds%iname('x'))
  print *, ds%index

  print *, 'print'
  call ds%print()

  print *, 'interpolate'
  ds2 = ds%interp([1.d0, 1.5d0,4.d0,4.3d0, 5.d0])
  print *, 'newaxis', ds2%index
  call ds2%print()

  print *, ''
  print *, 'copy'
  print *, '===='
  ds2 = ds%copy()
  call ds2%print()

  print *, ''
  print *, 'slice'
  print *, '====='
  ds2 = ds%slice(1,ds%nlen,2)
  call ds2%print()

  ! write(*,*) "index:",ds%index
  ! write(*,*) "getitem(v1):",ds%getitem('v1')
  ! write(*,*) 'x', ds%x
  ! write(*,*) 'y', ds%y
  ! write(*,*) 'sum(x)', sum(ds%x)
  ! write(*,*) 'mysum(x)', mysum(ds%x)  ! pointer but still works with functions written for arrays

  contains

    real(dp) function mysum(x)
      real(dp), intent(in) :: x(:)
      mysum = sum(x)
    end function mysum

end program 
