
main.x: main.f90 lifecycle.o field.o glider.o utilities.o
	f95 -o main.x main.f90 lifecycle.o field.o glider.o utilities.o

glider.o: glider.f90 utilities.o
	f95 -c glider.f90

lifecycle.o: lifecycle.f90
	f95 -c lifecycle.f90

field.o: field.f90
	f95 -c field.f90

utilities.o: utilities.f90
	f95 -c utilities.f90

run:main.x
	./main.x

clean: 
	rm *.mod *.o main.x
