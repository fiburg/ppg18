# Common definitions
CC = gfortran
THREADS = 4

# Compiler flags:
FLAGS =  -fopenmp -Wextra -O3
# flags for debugging:
#FLAGS =  -Wextra -Wall -g

poisson_Jacobi.x: mod_initialize.o mod_run_Jacobi.o mod_finalize.o poisson.o
	$(CC) $(FLAGS) -o poisson_Jacobi.x poisson.o mod_run_Jacobi.o mod_communicate.o mod_finalize.o

poisson.o: poisson.f90
	$(CC) $(FLAGS) -c poisson.f90

mod_initialize.o: mod_initialize.f90
	$(CC) $(FLAGS) -c mod_initialize.f90

mod_run_Jacobi.o: mod_run_Jacobi.f90
	$(CC) $(FLAGS) -c mod_run_Jacobi.f90

mod_finalize.o: mod_finalize.f90
	$(CC) $(FLAGS) -c mod_finalize.f90

run: poisson_Jacobi.x
	export OMP_NUM_THREADS=$(THREADS)
	/usr/bin/time ./poisson_Jacobi.x

clean:
	rm *.mod *.o *.x
