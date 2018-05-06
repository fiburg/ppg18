PROGRAM Poisson
	USE initialize
	!USE run
	!USE finalize
	IMPLICIT NONE
	double precision, dimension(:,:), allocatable :: matrix ! or whatever fits
	integer, parameter :: XDIM = 185, YDIM = 185

	call createMatrix(matrix, XDIM, YDIM)
	call initializeMatrix(matrix, XDIM, YDIM)
	
	!call calculate(matrix)

	!call outputMatrix(matrix)
	!call freeMatrix(matrix)



	
END PROGRAM Poisson
