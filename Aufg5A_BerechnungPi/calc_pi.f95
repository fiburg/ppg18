program CalculationPi
	
	use mpi
	
	implicit none
	
	integer(kind=8), parameter :: npieces = 10.e9, master = 0
	real(kind=8) :: num, sum, result, mypi
	integer(kind=8) :: i, ranki
	integer :: ierr, rank, size
	integer , dimension(MPI_STATUS_SIZE) :: status
	
	! individuell von Prozessoren aufaddiert
	integer, parameter :: msg = 21

	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
	call MPI_COMM_SIZE (MPI_COMM_WORLD, size,ierr)
	print*, 'I am', rank, 'of', size
 	
	sum = 0.0
	
	do i=rank+1, npieces, size
		num = (i-0.5) / npieces
		sum = sum + 4 / (1 + num * num)
	end do
	
	mypi = sum / npieces
	
! 	THE INDIVIDUAL SUM CASE
!
	if (rank == master) then
		result = mypi
		do ranki = 1, size-1
			call MPI_Recv(mypi, 1, MPI_DOUBLE_PRECISION, ranki, msg, MPI_COMM_WORLD, status, ierr)
			print*, 'received from rank ', ranki
			result = result + mypi
		end do
		
		write(6,*) result
		
	else
		call MPI_Send(mypi, 1, MPI_DOUBLE_PRECISION, master, msg, MPI_COMM_WORLD, ierr)
	endif
!
!	END OF INDIVIDUAL SUM CASE
	
	
!	THE REDUCE CASE
! 	
! 	call MPI_REDUCE(mypi, result, 1, MPI_DOUBLE_PRECISION, MPI_SUM, master, MPI_COMM_World, ierr)
! 	
! 	if (rank == 0) then
! 		write(6, *) result
! 	endif
!
!	END OF REDUCE CASE	
	
 	call MPI_FINALIZE(ierr)
 	
end program