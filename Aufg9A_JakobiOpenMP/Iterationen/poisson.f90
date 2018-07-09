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
	real(kind=8), parameter :: eps = 10.E-8	! geforderte Genauigkeit	
	integer, parameter :: NDIM = 96	! Dimension der Matrix
	integer, parameter :: interlines = 11	! Anzahl Interlines
	integer, parameter :: NITER = 100000	! Anzahl Iterationen
	integer :: iter	! ausgeführten Iterationen
	
	call createMatrix(matrix, NDIM, NDIM)
	call initializeMatrix(matrix)
	call outputMatrix(matrix, interlines, 0)
	
	do iter=1,NITER
		call calculate(matrix, eps)
	end do

	if (iter > NITER) iter = iter-1
	call outputMatrix(matrix, interlines, iter)
	call freeMatrix(matrix)

end program Poisson
