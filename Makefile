F08 = gfortran
FFLAGS = -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace -fbounds-check
SRCDIR = src
OBJDIR = .obj
BINDIR = bin
EXE = $(BINDIR)/a.out
LDFLAGS = -J./$(OBJDIR)

# SRC = $(wildcard $(SRCDIR)/*f90)
SRC = $(addprefix $(SRCDIR)/, utils.f90 dataset.f90 main.f90)
OBJ = $(SRC:$(SRCDIR)/%.f90=$(OBJDIR)/%.o)

$(EXE): $(OBJ)
	$(F08) $(OBJ) -o $@ $(LDFLAGS)

$(OBJ): $(OBJDIR)/%.o : $(SRCDIR)/%.f90
	$(F08) -c $< -o $@ $(LDFLAGS) $(FFLAGS)

clean:
	rm $(EXE) $(OBJDIR)/* -f
