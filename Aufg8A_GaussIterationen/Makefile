# Common definitions
CC = mpif90
EX = mpiexec
NP = 5

# Compiler flags:
FLAGS =  -Wextra -O3
# flags for debugging:
#FLAGS =  -Wextra -Wall -g

poisson_Gauss.x: mod_initialize.o mod_run_Gauss.o mod_communicate.o mod_finalize.o poisson.o
	$(CC) $(FLAGS) -o poisson_Jacobi.x poisson.o mod_initialize.o mod_run_Gauss.o mod_communicate.o mod_finalize.o

poisson.o: poisson.f90
	$(CC) $(FLAGS) -c poisson.f90

mod_initialize.o: mod_initialize.f90
	$(CC) $(FLAGS) -c mod_initialize.f90

mod_communicate.o: mod_communicate.f90
	$(CC) $(FLAGS) -c mod_communicate.f90

mod_run_Gauss.o: mod_run_Gauss.f90
	$(CC) $(FLAGS) -c mod_run_Gauss.f90

mod_finalize.o: mod_finalize.f90
	$(CC) $(FLAGS) -c mod_finalize.f90

run: poisson_Jacobi.x
	$(EX) -n $(NP) ./poisson_Gauss.x

clean:
	rm *.mod *.o *.x
