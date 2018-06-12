! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 6
! Abgabe 12.06.2018
! Menken und Burgemeister

! Allokation und Initialisierung der Matrix
module initialize
	implicit none
	contains
	
	! Allokation der Matrix mit Initialisierung auf 0.
	subroutine createMatrix(matrix, xdim, ydim)
		implicit none
		integer, intent(in) :: xdim, ydim 
		integer, dimension(:,:), pointer, intent(inout) :: matrix
		
		allocate(matrix(1:xdim,1:ydim))
		
		matrix(:,:) = 0.

	end subroutine createMatrix
	
	! Initialiserung der Matrix
	subroutine initializeMatrix(matrix)
		implicit none
		integer, dimension(:,:), pointer, intent(inout) :: matrix
		integer :: i, j, xdim, ydim, seq
		
		xdim = ubound(matrix, 1)
		ydim = ubound(matrix, 2)

		seq = xdim * ydim

		do i=1,xdim
			do j=1,ydim
				matrix(j,i) = seq
				seq = seq - 1
			end do
		end do

	end subroutine initializeMatrix

end module initialize
