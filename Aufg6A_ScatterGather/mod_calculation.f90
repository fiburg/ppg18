! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 6
! Abgabe 12.06.2018
! Menken und Burgemeister

! Manipulation der Matrix
module calculation
	implicit none
	contains
	
	! Durchfuehrung der Operation N(i,j) = N(i,j)*(ProzessorID + 1)
	subroutine operationSequence(chunck, process)
		implicit none
		integer, dimension(:,:), pointer, intent(inout) :: chunck
		integer, intent(in) :: process
		integer :: i, j, xdim, ydim
		
		xdim = ubound(chunck, 1)
		ydim = ubound(chunck, 2)

		do i=1,xdim
			do j=1,ydim
				chunck(i,j) = chunck(i,j) * (process + 1)
			end do
		end do

	end subroutine operationSequence

end module calculation
