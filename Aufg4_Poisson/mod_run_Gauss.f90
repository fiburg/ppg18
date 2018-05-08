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
		real(kind=8) :: star = 0., corr = 0., diff = 0.
		integer, intent(in) :: NDIM
		integer, parameter :: NITER = 40000	! Anzahl Iterationen
		real(kind=8), parameter :: eps = 1.E-6	! geforderte Genauigkeit
		integer :: i, j
		integer, intent(out) :: iter

		write(*,*) "It will use Gauß-Seidel Method"
		
		do iter=1,NITER
			! Zuruecksetzen
			diff = 0.
			star = 0.
			corr = 0.

			do i=2,NDIM
				do j=2,NDIM
					! Abtaststern
					star = -matrix(i,j+1) - matrix(i-1,j) &
					&	+ 4 * matrix(i,j)	&
					&	- matrix(i+1,j)	- matrix(i,j-1)
					
					! Korrekturwert
					corr = (matrix(i,j) &
					&	* 1./NDIM * 1./NDIM - star) / 4.
					
					! Differenz alt neu
					diff = diff + corr

					! Korrektur
					matrix(i,j) = matrix(i,j) + corr
				end do
			end do

			! mittlere Differenz
			diff = diff / ((NDIM-1) * (NDIM-1))

			if(diff < eps) exit
		end do

		iter = iter-1
	
	end subroutine calculate

end module run
