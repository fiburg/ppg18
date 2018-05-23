visual.x:visual.f95
	scorep mpif90 -o visual.x visual.f95

run: visual.x
	mpiexec -n 8 ./visual.x

clean: 
	rm -r scorep* visual.x