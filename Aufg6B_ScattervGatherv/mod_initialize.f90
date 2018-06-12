! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 6
! Abgabe 12.06.2018
! Menken und Burgemeister

! Allokation und Initialisierungen
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
				matrix(i,j) = seq
				seq = seq - 1
			end do
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
