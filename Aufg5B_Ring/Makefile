ring.x:ring.f95
	mpif90 -o ring.x ring.f95

run: ring.x
	mpiexec -n 8 ./ring.x

clean: 
	rm ring.x