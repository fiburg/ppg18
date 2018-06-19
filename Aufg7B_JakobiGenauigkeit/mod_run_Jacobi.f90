! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 7
! Abgabe 19.06.2018
! Menken und Burgemeister

module run
	use initialize
	implicit none
	contains
	
	! Loesung der Poissongleichung mittels Jacobi-Verfahren
	subroutine calculate(matrix, eps, acc)
		implicit none
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix
		real(kind=8), dimension(:,:), pointer :: old
		real(kind=8) :: star = 0., corr = 0.
		real(kind=8), parameter :: eps = 10.E-6	! geforderte Genauigkeit
		logical :: acc = .false. ! Abbruchbedingung 
		integer :: xdim, ydim
		integer :: i, j
		integer :: ndiff = 0

		xdim = ubound(matrix, 1)
		ydim = ubound(matrix, 2)

		call createMatrix(old, xdim, ydim)

		old(:,:) = matrix(:,:)

		do i=1,xdim-1
			do j=1,ydim-1
				! Abtaststern
				star = -old(i,j+1) - old(i-1,j) &
				&	+ 4. * old(i,j)	&
				&	- old(i+1,j) - old(i,j-1)
				
				! Korrekturwert, hier Stoerfunktion Null
				corr = - star / 4.

				! Korrektur
				matrix(i,j) = matrix(i,j) + corr

				! einzelne Abbruchbedingung
				if(abs(corr) < eps) ndiff = ndiff + 1
			end do
		end do

		! teste ueberall Genauigkeit erreicht
		if(ndiff==((NDIM-1)*(NDIM-1))) fin = .true.

		deallocate(old)
		
	end subroutine calculate

end module run
