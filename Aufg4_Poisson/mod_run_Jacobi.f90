! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 4
! Abgabe 15.05.2018
! Menken und Burgemeister

module run
	use initialize
	implicit none
	contains
	
	! Loesung der Poissongleichung mittels Gauss-Seidel-Verfahren
	subroutine calculate(matrix, NDIM, iter)
		implicit none
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix
		real(kind=8), dimension(:,:), pointer :: old
		real(kind=8) :: star = 0., corr = 0.
		real(kind=8), parameter :: eps = 10.E-6	! geforderte Genauigkeit
		integer, intent(in) :: NDIM
		integer, parameter :: NITER = 400000	! Anzahl Iterationen
		integer :: i, j
		integer, intent(out) :: iter
		integer :: ndiff = 0
		
		write(*,*) "It will use Jacobi Method"

		call createMatrix(old, NDIM)

		do iter=1,NITER
			! Zuruecksetzen
			star = 0.
			corr = 0.
			ndiff = 0
			old(:,:) = matrix(:,:)

			do i=1,NDIM-1
				do j=1,NDIM-1
					! Abtaststern
					star = -old(i,j+1) - old(i-1,j) &
					&	+ 4. * old(i,j)	&
					&	- old(i+1,j) - old(i,j-1)
					
					! Korrekturwert, hier Stoerfunktion Null
					corr = - star / 4.

					! Korrektur
					matrix(i,j) = matrix(i,j) + corr
					
					! Abbruchbedingung
					if(abs(corr) < eps) ndiff = ndiff + 1					
					
				end do
			end do

			if(ndiff==((NDIM-1)*(NDIM-1))) exit
		end do

		deallocate(old)
		
	end subroutine calculate

end module run
