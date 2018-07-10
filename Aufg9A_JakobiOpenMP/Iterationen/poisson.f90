! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 9
! Abgabe 10.07.2018
! Menken und Burgemeister

program Poisson
	use initialize
	use run
	use finalize

	implicit none

	real(kind=8), dimension(:,:), pointer :: matrix
	integer, parameter :: NDIM = 184	! Dimension der Matrix
	integer, parameter :: interlines = 22	! Anzahl Interlines
	integer, parameter :: NITER = 400000	! Anzahl Iterationen
	integer :: iter	! ausgeführten Iterationen
	
	call createMatrix(matrix, NDIM, NDIM)
	call initializeMatrix(matrix)
	call outputMatrix(matrix, interlines, 0)
	
	do iter=1,NITER
		call calculate(matrix)
	end do

	if (iter > NITER) iter = iter-1
	call outputMatrix(matrix, interlines, iter)
	call freeMatrix(matrix)

end program Poisson
