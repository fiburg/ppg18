
main.x: main.f90 lifecycle.o field.o utilities.o figure.o
	f95 -g -o main.x main.f90 lifecycle.o field.o utilities.o figure.o

field.o: field.f90 figure.o
	f95 -c field.f90

figure.o: figure.f90
	f95 -c figure.f90

lifecycle.o: lifecycle.f90
	f95 -c lifecycle.f90

utilities.o: utilities.f90
	f95 -c utilities.f90

run: main.x
	./main.x

debug: main.f90 lifecycle.o field.o utilities.o figure.o
	f95 -g -o main.x main.f90 lifecycle.o field.o utilities.o figure.o

clean: 
	rm *.mod *.o main.x
