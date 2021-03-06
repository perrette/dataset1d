program main

  use types, only: clen, dp
  use dataset_mod, only: Dataset

  type(Dataset) :: ds, ds2

  print *, '============'
  print *, 'Test Dataset'
  print *, '============'

  print *, ''
  print *, 'alloc'
  print *, '====='
  call ds%alloc(nlen=5, nvar=3)
  call ds%print()

  print *, ''
  print *, 'define variable names'
  print *, '====================='
  ds%names(1) = "x"
  ds%names(2) = "v1"
  ds%names(3) = "v2"

  print *, ''
  print *, 'setitem/getitem'
  print *, '==============='
  call ds%setitem('x', [1.d0, 2.d0, 3.d0, 4.d0, 5.d0])
  call ds%setitem('v1',ds%getitem('x')**2)
  call ds%setitem('v2',ds%getitem('x')+0.d0)
  print *, ds%getitem('v1',start=1,stop_=ds%nlen,step=2)

  print *, ''
  print *, 'set index'
  print *, '========='
  call ds%set_index(ds%iname('x'))
  print *, ds%index

  print *, ''
  print *, 'print'
  print *, '====='
  call ds%print()
  print *, 'large variable'
  print *, '=============='
  call ds2%alloc(nlen=1000000, nvar=5)
  call ds2%print()

  print *, ''
  print *, 'interpolate on ',[1.d0, 1.5d0,4.d0,4.3d0, 5.d0]
  print *, '==========='
  ds2 = ds%interp([1.d0, 1.5d0,4.d0,4.3d0, 5.d0])
  call ds2%print()

  print *, ''
  print *, 'copy'
  print *, '===='
  ds2 = ds%copy()
  call ds2%print()

  print *, ''
  print *, 'slice ::2'
  print *, '========='
  ds2 = ds%slice(1,ds%nlen,2)
  call ds2%print()

  print *, ''
  print *, 'compress: x > 3'
  print *, '==============='
  ds2 = ds%compress(ds%getitem('x')>3)
  call ds2%print()

  print *, ''
  print *, 'take x, v1 variables'
  print *, '===================='
  ds2 = ds%take([ds%iname('x'), ds%iname('v1')], axis=2)
  call ds2%print()

  print *, ''
  print *, 'take elements 1,4,5'
  print *, '==================='
  ds2 = ds%take([1,4,5])
  call ds2%print()

  print *, ''
  print *, 'write to netCDF'
  print *, '==============='
  call ds%write_nc('test.nc')

  print *, ''
  print *, 'read from netCDF'
  print *, '================'
  call ds2%read_nc('test.nc')
  call ds2%print()

  contains

    real(dp) function mysum(x)
      real(dp), intent(in) :: x(:)
      mysum = sum(x)
    end function mysum

end program 
