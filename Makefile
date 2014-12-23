F08 = gfortran
SRCDIR = src
OBJDIR = .obj
BINDIR = bin
EXE = $(BINDIR)/a.out
# SRC = $(wildcard $(SRCDIR)/*f90)
SRC = $(addprefix $(SRCDIR)/, ncio.f90 utils.f90 dataset.f90 main.f90)
OBJ = $(SRC:$(SRCDIR)/%.f90=$(OBJDIR)/%.o)

# for netCDF
LIB = /usr/lib
INC = /usr/include

# compilation options
debug=1
ifeq ($(debug),1)
    DFLAGS = -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fbounds-check
else
    DFLAGS   = -O3
endif

# compilation and linking flags
FLAGS = -J./$(OBJDIR) -I$(INC)
LFLAGS = -L$(LIB) -lnetcdff -lnetcdf

$(EXE): $(OBJ)
	$(F08) $(OBJ) -o $@ $(LFLAGS)

$(OBJ): $(OBJDIR)/%.o : $(SRCDIR)/%.f90
	$(F08) -c $< -o $@ $(DFLAGS) $(FLAGS)

clean:
	rm $(EXE) $(OBJDIR)/* -f
