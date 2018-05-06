module initialize
	implicit none
	contains
	
	subroutine createMatrix(matrix, NDIM)
		implicit none
		integer, intent(in) :: NDIM 
		double precision, dimension(:,:), allocatable, intent(inout) :: matrix
		
		!create matrix

		allocate(matrix(1:XDIM,1:YDIM))
		
		matrix(:,:) = 0.

	end subroutine createMatrix
	
	subroutine initializeMatrix(matrix, NDIM)
		implicit none
		integer, intent(in) :: NDIM
		double precision, dimension(:,:), intent(inout) :: matrix		
		integer :: i
		double precision :: width
		
		!initialize matrix
		
		width = 1. / NDIM
		
		! Randwerte
		do i=1,NDIM
			matrix(i,1) = 1-(i-1)*width
			matrix(i,NDIM) = (i-1)*width
			matrix(1,i) = 1-(i-1)*width
			matrix(NDIM,i) = (i-1)*width
		end do

		! wirklich auf Null setzen, PRUEFEN
		matrix(1,YDIM) = 0.0
		matrix(XDIM,1) = 0.0
		
	end subroutine initializeMatrix

end module initialize
