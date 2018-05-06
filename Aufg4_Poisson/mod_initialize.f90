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

		allocate(matrix(0:NDIM,0:NDIM))
		
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
		do i=0,NDIM
			matrix(i,0) = 1-i*width
			matrix(i,NDIM) = i*width
			matrix(0,i) = 1-i*width
			matrix(NDIM,i) = i*width
		end do

		! wirklich auf Null setzen, PRUEFEN
		matrix(0,NDIM) = 0.0
		matrix(NDIM,0) = 0.0
		
	end subroutine initializeMatrix

end module initialize
