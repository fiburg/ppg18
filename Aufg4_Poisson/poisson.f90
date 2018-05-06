PROGRAM Poisson
	USE initialize
	USE run
	USE finalize
	IMPLICIT NONE
	double precision, dimension(:,:), pointer :: matrix
	integer, parameter :: NDIM = 184

	call createMatrix(matrix, NDIM)
	call initializeMatrix(matrix, NDIM)
	
	!call calculate(matrix)

	call outputMatrix(matrix)
	call freeMatrix(matrix)

END PROGRAM Poisson
