! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 4
! Abgabe 15.05.2018
! Menken und Burgemeister

! Allokation und Initialisierung der Matrix
module initialize
	implicit none
	contains
	
	! Allokation der Matrix mit Initialisierung auf 0.
	subroutine createMatrix(matrix, NDIM)
		implicit none
		integer, intent(in) :: NDIM 
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix
		
		!create matrix

		allocate(matrix(1:NDIM+1,1:NDIM+1))
		
		matrix(:,:) = 0.

	end subroutine createMatrix
	
	! Initialiserung der Matrix mit Randwerten
	subroutine initializeMatrix(matrix, NDIM)
		implicit none
		integer, intent(in) :: NDIM
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix		
		real(kind=8) :: width
		integer :: i
		
		width = 1. / NDIM
		
		! Randwerte
		matrix(1,1) = 1.
		matrix(NDIM+1,NDIM+1) = 1.
		matrix(1,NDIM+1) = 0.
		matrix(NDIM+1,1) = 0.		

		do i=2,NDIM
			matrix(i,1) = 1-i*width
			matrix(i,NDIM+1) = i*width
			matrix(1,i) = 1-i*width
			matrix(NDIM+1,i) = i*width
		end do

	end subroutine initializeMatrix

end module initialize
