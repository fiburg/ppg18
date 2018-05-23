calc_pi.x:calc_pi.f95
	mpif90 -o calc_pi.x calc_pi.f95

run: calc_pi.x
	mpiexec -n 4 ./calc_pi.x

clean: 
	rm calc_pi.x