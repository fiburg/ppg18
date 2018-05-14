! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 4
! Abgabe 15.05.2018
! Menken und Burgemeister

! Ausgabe der Matrix und Freigabe des Speichers
module finalize
	implicit none
	contains

	! Deallokierung der Matrix
	subroutine freeMatrix(matrix)
		implicit none
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix

		deallocate(matrix)
		
	end subroutine freeMatrix

	! Ausgabe der Berechnungsmatrix auf 9x9 Matrix
	subroutine outputMatrix(matrix, iter)
		implicit none
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix
		real(kind=8), dimension(0:8,0:8) :: matrix_out
		integer, parameter :: interlines = 22
		integer :: i, j, ix, iy
		integer :: iter

		write(*, '(A13,1X,I6,1X,A10)') 'Results after',iter,'iterations'

		do i=0,8
			do j=0,8
				matrix_out(i,j) = matrix( &
				&	i*(interlines+1), &
				&	j*(interlines+1))			
				write(*, '(f9.7,2X)', advance='no') matrix_out(i,j)
			end do
			write(*,*) ''

		end do
		
	end subroutine outputMatrix

end module finalize
