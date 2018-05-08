MODULE run
	IMPLICIT NONE
	CONTAINS
	
	SUBROUTINE calculate(matrix) !more parameters as needed
		IMPLICIT NONE
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix
		integer, parameter :: niter = 40000	! number of iterations
		real(kind=8), parameter :: eps = 1.E-6	! minimal error reduction
		! calculate with Gauß-Seidel Method
		write(*,*) "It will use Gauß-Seidel Method"
		
	END SUBROUTINE calculate

END MODULE run
