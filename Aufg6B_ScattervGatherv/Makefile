# Common definitions
CC = mpif90
EX = mpiexec
NP = 7

main.x: mod_initialize.o mod_calculation.o main.o
	$(CC) -g -o main.x main.o mod_initialize.o mod_calculation.o

main.o: main.f90
	$(CC) -c main.f90
	
mod_initialize.o: mod_initialize.f90
	$(CC) -c mod_initialize.f90

mod_calculation.o: mod_calculation.f90
	$(CC) -c mod_calculation.f90

run: main.x
	$(EX) -n $(NP) ./main.x

clean: 
	rm *.mod *.o *.x
