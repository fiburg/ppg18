! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 8
! Abgabe 28.06.2018
! Menken und Burgemeister

! Parallelisierung der Berechnung von Pi mit OpenMP.

program CalculationPi
	use omp_lib
	
	implicit none
	
	integer(kind=8), parameter :: npieces = 10.e9
	real(kind=8) :: x, f, sum, mypi
	integer(kind=8) :: i
 	
 	! set number of threads
 	call omp_set_num_threads(4)
	
	! calculation of partial results
	sum = 0.0
		
	! parallelisierte do-Schleife:
	! testing omp functionality	
	!$omp parallel
	print*, "hello from", omp_get_thread_num(), "out of", omp_get_num_threads()
	!$omp end parallel
	
	
	!$omp parallel do private(x,f) &
	!$omp reduction(+:sum)
	do i=1, npieces
		x = (i-0.5) / npieces
		f = 4 / (1 + x * x)
		sum = sum + f
	end do
	!$omp end parallel do
	
	mypi = sum / npieces
	
	print*, "Calculated Pi is ",mypi
	
end program

