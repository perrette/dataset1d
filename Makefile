a.out: dataset.f90
	gfortran -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fbounds-check dataset.f90 -o a.out

clean:
	rm a.out dataset_mod.mod  types.mod
