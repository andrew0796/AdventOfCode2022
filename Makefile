# Advent of Code 2022 Makefile


.PHONY: all newday doc clean

FC := gfortran

SRC := $(wildcard src/*.f90)
BIN := $(patsubst src/%.f90, %.out, $(SRC))

TMP := src/template.f90

all: ${BIN}

%.out: src/%.f90
	$(FC) $< -o $@

newday:
ifeq ("$(wildcard src/day$(DAY).f90)", "")
	@echo "Creating new source file for day $(DAY)"
	@sed 's/N/$(DAY)/g' $(TMP) > src/day$(DAY).f90
else
	@echo "Source file for day $(DAY) already exists"
endif

ifeq ("$(wildcard data/day$(DAY).text)", "")
	@echo "Creating new data file for day $(DAY)"
	@touch data/day$(DAY).txt
else
	@echo "Data file for day $(DAY) already exists"
endif

ifeq ("$(wildcard data/test$(DAY).text)", "")
	@echo "Creating new test data file for day $(DAY)"
	@touch data/test$(DAY).txt
else
	@echo "Test data file for day $(DAY) already exists"
endif

doc:
	@echo "Advent of Code 2022"
	@echo "Run with no arguments to compile code"
	@echo "clean:\n\tRemoves all executables and modules"
	@echo "newday DAY=N:\n\tCreates a template file for day N only if src/dayN.f90 does not exist"

clean:
	@echo "Removing executables and modules"
	@rm -r *.out *.mod
