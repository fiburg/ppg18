! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 4
! Abgabe 15.05.2018
! Menken und Burgemeister

module run
	implicit none
	contains
	
	! Loesung der Poissongleichung mittels Gauss-Seidel-Verfahren
	subroutine calculate(matrix, NDIM, iter)
		implicit none
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix
		real(kind=8) :: star = 0., corr = 0.
		real(kind=8), parameter :: eps = 10.e-6 ! geforderte Genauigkeit
		integer, intent(in) :: NDIM
		integer, parameter :: NITER = 400000	! Anzahl Iterationen
		integer, intent(out) :: iter
		integer :: i, j
		integer :: ndiff = 0


		write(*,*) "It will use Gauß-Seidel Method"
		
		do iter=1,NITER
			! Zuruecksetzen
			star = 0.
			corr = 0.
			ndiff = 0

			do i=1,NDIM-1
				do j=1,NDIM-1
					! Abtaststern
					star = -matrix(i,j+1) - matrix(i-1,j) &
					&	+ 4 * matrix(i,j)	&
					&	- matrix(i+1,j)	- matrix(i,j-1)
					
					! Korrekturwert, hier Stoerfunktion Null
					corr = - star / 4.

					! Abbruchbedingung
					if(corr < eps) ndiff = ndiff + 1					

					! Korrektur
					matrix(i,j) = matrix(i,j) + corr
				end do
			end do

			if(ndiff==((NDIM-1)*(NDIM-1))) exit
		end do

		iter = iter-1

		end subroutine calculate

end module run
