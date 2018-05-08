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
	subroutine outputMatrix(matrix)
		implicit none
		real(kind=8), dimension(:,:), pointer, intent(inout) :: matrix
		real(kind=8), dimension(1:9,1:9) :: matrix_out
		integer, parameter :: interlines = 22
		integer :: i, j

		do i=1,9
			do j=1,9
				matrix_out(i,j) = matrix( &
				&	1+(i-1)*(interlines+1), &
				&	1+(j-1)*(interlines+1))			
			
			end do
			write(*,*) matrix_out(i,:)

		end do
		
	end subroutine outputMatrix

end module finalize
