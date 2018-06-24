! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 8
! Abgabe 26.06.2018
! Menken und Burgemeister

module run
	implicit none
	contains
	
	! Loesung der Poissongleichung mittels Gauss-Seidel-Verfahren
	subroutine calculate(matrix, eps, acc)
		implicit none
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix
		real(kind=8) :: star = 0., corr = 0.
		real(kind=8) :: eps	! geforderte Genauigkeit
		logical :: acc	! Abbruchbedingung 
		integer :: xdim, ydim
		integer :: i, j
		integer :: ndiff = 0
	
		xdim = ubound(matrix, 1)
		ydim = ubound(matrix, 2)

		star = 0.
		corr = 0.
		ndiff = 0

		do i=1,xdim-1
			do j=1,ydim-1
				! Abtaststern
				star = -matrix(i,j+1) - matrix(i-1,j) &
				&	+ 4 * matrix(i,j)	&
				&	- matrix(i+1,j)	- matrix(i,j-1)
				
				! Korrekturwert, hier Stoerfunktion Null
				corr = - star / 4.

				! Korrektur
				matrix(i,j) = matrix(i,j) + corr

				! einzelne Abbruchbedingung
				if(abs(corr) < eps) ndiff = ndiff + 1
			end do
		end do

		! teste ueberall Genauigkeit erreicht
		if(ndiff==((xdim-1)*(ydim-1))) acc = .true.

	end subroutine calculate

end module run
