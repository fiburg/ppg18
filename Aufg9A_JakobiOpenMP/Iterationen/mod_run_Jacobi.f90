! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 9
! Abgabe 10.07.2018
! Menken und Burgemeister

module run
	use omp_lib
	use initialize
	implicit none
	contains
	
	! Loesung der Poissongleichung mittels Jacobi-Verfahren
	subroutine calculate(matrix)
		implicit none
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix
		real(kind=8), dimension(:,:), pointer :: old
		real(kind=8) :: star, corr
		integer :: xdim, ydim
		integer :: i, j

		xdim = ubound(matrix, 1)
		ydim = ubound(matrix, 2)

		call createMatrix(old, xdim, ydim)

		old(:,:) = matrix(:,:)

		star = 0.
		corr = 0.
		
		!$OMP PARALLEL DO DEFAULT(NONE) &
		!$OMP SHARED (xdim,ydim,old,eps,matrix) &
		!$OMP PRIVATE(i,j,star,corr) &
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

			end do
		end do
		!$OMP END PARALLEL DO

		deallocate(old)
		
	end subroutine calculate

end module run
