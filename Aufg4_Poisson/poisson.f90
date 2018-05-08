PROGRAM Poisson
	USE initialize
	USE run
	USE finalize
	IMPLICIT NONE
	real(kind=8), dimension(:,:), pointer :: matrix
	integer, parameter :: NDIM = 184
	integer :: iter

	call createMatrix(matrix, NDIM)
	call initializeMatrix(matrix, NDIM)
	call outputMatrix(matrix, 0)	

	call calculate(matrix, NDIM, iter)

	call outputMatrix(matrix, iter)
	call freeMatrix(matrix)

END PROGRAM Poisson
