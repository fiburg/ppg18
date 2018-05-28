! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 5
! Abgabe 29.05.2018
! Menken und Burgemeister

! Ring - Send and receive, process 0 sends the process id to process 1,
! process 1 to process 2 .... The result will be the sum of each process id. 
program ring

	use mpi
	
	implicit none
	
	integer :: ierr, rank, size	! error, process id and num of process
	integer :: status(MPI_STATUS_SIZE)	! for MPI receiving
	integer, parameter :: msg = 12	! for MPI receiving and sending
	integer :: sum_id	! updated sum of process ids
	integer :: recp	! recipient of MPI sending
	
	! initialisation of MPI
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
	call MPI_COMM_SIZE (MPI_COMM_WORLD, size,ierr)
	print*, 'I am', rank, 'of', size

	! first process starts the ring communication
	if (rank==0) then
		sum_id = rank
		call MPI_Send(sum_id, 1, MPI_INTEGER, rank+1, msg, &
		&	      MPI_COMM_WORLD, ierr)
		
		! write the added up process ids after going through all 
		! process and ending up again at the first process
		call MPI_Recv(sum_id, 1, MPI_INTEGER, size-1, msg, &
		&	      MPI_COMM_WORLD, status, ierr)
		write(*,*) "Added up process ids: ", sum_id
	
	else
		! receive the current value of sum_id from the process before
		call MPI_Recv(sum_id, 1, MPI_INTEGER, rank-1, msg, &
		&	      MPI_COMM_WORLD, status, ierr)
		print*, rank, "received"
		
		if (rank < size-1) then
			! if it's not the last process set the recipient to 
			! the next process
			recp = rank+1
		else
			! if it's the last process set the recipient to the 
			! first process
			recp = 0
		endif
		
		! send the current sum of ids plus the process id
		call MPI_Send(sum_id+rank, 1, MPI_INTEGER, recp, msg, &
		&	      MPI_COMM_WORLD, ierr)

	endif
	
	call MPI_FINALIZE(ierr)
	
	! RESULTS
	! 4 proc : 6
	! 8 proc : 28
	! 11 proc : 55
	
end program
