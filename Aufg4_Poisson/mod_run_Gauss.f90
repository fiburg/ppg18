MODULE run
	IMPLICIT NONE
	CONTAINS
	
	SUBROUTINE calculate(matrix, NDIM, iter)
		IMPLICIT NONE
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix
		real(kind=8) :: star = 0., corr = 0., diff = 0.
		integer, intent(in) :: NDIM
		integer, parameter :: NITER = 40000	! Anzahl der Iterationen
		real(kind=8), parameter :: eps = 1.E-6	! geforderte Genauigkeit
		integer :: i, j
		integer, intent(out) :: iter

		! calculate with Gauß-Seidel Method
		write(*,*) "It will use Gauß-Seidel Method"
		
		do iter=1,NITER
			do i=2,NDIM
				do j=2,NDIM
					star = -matrix(i,j+1) - matrix(i-1,j) + 4 * matrix(i,j) - matrix(i+1,j) - matrix(i,j-1)
					corr = (matrix(i,j) * 1./NDIM * 1./NDIM - star) / 4.
					diff = diff + corr
					matrix(i,j) = matrix(i,j)+corr
				end do
			end do

			diff = diff / ((NDIM-1) * (NDIM-1))

			if(diff < eps) exit
		end do

		iter = iter-1
	
	END SUBROUTINE calculate

END MODULE run
