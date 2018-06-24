! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 7
! Abgabe 19.06.2018
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

	! Generische Berechnung von Sendcount und Displacement fuer Scatterv
	subroutine initSequence(sendcounts, displacement, mdim, mpi_size)
		implicit none
		integer, dimension(:), pointer, intent(inout) :: sendcounts, displacement
		integer, intent(in) :: mdim, mpi_size
		integer :: i
		
		allocate(sendcounts(1:mpi_size))
		allocate(displacement(1:mpi_size))
		
		sendcounts = mdim * int(mdim / mpi_size)
		sendcounts(mpi_size) = mdim * (int(mdim / mpi_size) + mod(mdim, mpi_size))
		
		displacement(1) = 0
		do i=2,mpi_size 
			displacement(i) = displacement(i-1) + sendcounts(i-1)
		end do

	end subroutine initSequence

end module initialize
