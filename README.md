dataset1d
=========

 A handy in-memory netCDF Dataset for fortran, for 1-D arrays only (so making it more similar to a pandas DataFrame).

Useful commands:

    use dataset, only: Dataset
    type(Dataset) :: ds

Allocate memory for an underlying ds%values(:,:) pointer

    call ds%alloc(nvar=3, nlen=5)

Define variable names (default to "v1", "v2"...)

    ds%names(1) = "x"
    ds%names(2) = "v1"
    ds%names(3) = "v2"

Can set and get variables by name

    call ds%setitem('x', [1.d0, 2.d0, 3.d0, 4.d0, 5.d0])
    call ds%setitem('v1',ds%getitem('x')**2)

Or directly access the underlying `values` attribute, using the `iname` method
to obtain the integer position index corresponding to a variable name.

    call ds%values(:, ds%iname('v2')) = ds%values(:, ds%iname('x'))+0.d0

Includes pretty printing or arrays a la pandas' DataFrame

    call ds%print()

    Dataset(nvar=3, nlen=5)
              x          v1          v2 
      1  1.0000E+00 1.0000E+00 1.0000E+00
      2  2.0000E+00 4.0000E+00 2.0000E+00
      3  3.0000E+00 9.0000E+00 3.0000E+00
      4  4.0000E+00 1.6000E+01 4.0000E+00
      5  5.0000E+00 2.5000E+01 5.0000E+00

As well as netCDF I/O

    call ds%write_nc('test.nc')
    call ds%read_nc('test.nc')

And some basic functionality such as interpolation

    call ds%set_index(ds%iname("x"))  ! set the x variable as index
    ds2 = ds%interp([1.d0, 1.5d0,4.d0]) ! interpolate on new axis

    Dataset(nvar=3, nlen=3)
              x          v1          v2 
      1  1.0000E+00 1.0000E+00 1.0000E+00
      2  1.5000E+00 2.5000E+00 1.5000E+00
      3  4.0000E+00 1.6000E+01 4.0000E+00

It is also possible to use methods such as slice, compress, take, to index the 
dataset as a whole along the first (index) or second (variable) dimension.

More functions such as align or reidnex could be built in.
The current limitation is that only a single type can be represented in the 
DataFrame (currently double precision), but this has the advantage of being 
efficient. A more, complex, mixed-type and mixed-dimensions dataset will be 
presented in another project.
