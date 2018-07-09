! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 9
! Abgabe 10.07.2018
! Menken und Burgemeister

! Allokation und Initialisierung der Matrix
module initialize
	implicit none
	contains
	
	! Allokation der Matrix mit Initialisierung auf 0.
	subroutine createMatrix(matrix, xdim, ydim)
		implicit none
		integer, intent(in) :: xdim, ydim 
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix
		
		allocate(matrix(0:xdim,0:ydim))
		
		matrix(:,:) = 0.

	end subroutine createMatrix
	
	! Initialiserung der Matrix mit Randwerten
	subroutine initializeMatrix(matrix)
		implicit none
		integer :: xdim, ydim
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix		
		real(kind=8) :: xwidth, ywidth
		integer :: i
		
		xdim = ubound(matrix, 1)
		ydim = ubound(matrix, 2)

		xwidth = 1. / xdim
		ywidth = 1. / ydim

		! Randwerte
		matrix(0,0) = 1.
		matrix(xdim,ydim) = 1.
		matrix(0,ydim) = 0.
		matrix(xdim,0) = 0.		

		do i=1,xdim-1
			matrix(i,0) = 1.-i*xwidth
			matrix(i,ydim) = i*xwidth
		end do

		do i=1,ydim-1
			matrix(0,i) = 1.-i*ywidth
			matrix(xdim,i) = i*ywidth
		end do		

	end subroutine initializeMatrix

end module initialize
