
FC=mpifrtpx

all: build run
build: test.f90  mpi_spawn.f90
	$(FC) -o test.exe test.f90
	$(FC) -o mpi_spawn.exe mpi_spawn.f90

run:
	mpiexec -n 3 ./mpi_spawn.exe 95


