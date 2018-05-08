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
		real(kind=8) :: star = 0., corr = 0., diff = 0.
		real(kind=8), parameter :: eps = 1.E-6	! geforderte Genauigkeit
		integer, intent(in) :: NDIM
		integer, parameter :: NITER = 40000	! Anzahl Iterationen
		integer :: i, j
		integer, intent(out) :: iter
		
		write(*,*) "It will use Jacobi Method"

		call createMatrix(old, NDIM)

		do iter=1,NITER
			! Zuruecksetzen
			diff = 0.
			star = 0.
			corr = 0.
			old(:,:) = matrix(:,:)

			do i=2,NDIM
				do j=2,NDIM
					! Abtaststern
					star = -old(i,j+1) - old(i-1,j) &
					&	+ 4 * old(i,j)	&
					&	- old(i+1,j)	- old(i,j-1)
					
					! Korrekturwert
					corr = (old(i,j) &
					&	* 1./NDIM * 1./NDIM - star) / 4.

					! Korrektur
					matrix(i,j) = matrix(i,j) + corr
					
					! Differenz alt neu
					diff = diff + (matrix(i,j) - old(i,j))
				end do
			end do

			! mittlere Differenz
			diff = diff / ((NDIM-1) * (NDIM-1))

			if(diff < eps) exit
		end do

		iter = iter-1

		deallocate(old)
		
	end subroutine calculate

end module run
