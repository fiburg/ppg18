program ring

	use mpi
	
	implicit none
	
	integer :: ierr, rank, size	! error, processor id and num of processors
	integer , dimension(MPI_STATUS_SIZE) :: status	! for MPI receiving
	integer, parameter :: msg = 12	! for MPI receiving and sending
	integer :: sum_id	! updated sum of process ids
	integer :: recp	! recipient of MPI sending
	
	! initialisation of MPI
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
	call MPI_COMM_SIZE (MPI_COMM_WORLD, size,ierr)
	
	! first processor starts the ring communication
	if (rank==0) then
		sum_id = rank
		call MPI_Send(sum_id, 1, MPI_INTEGER, rank+1, msg, MPI_COMM_WORLD, ierr)
		
		! write the added up process ids after going through all processors and ending up
		! again at the first processor
		call MPI_Recv(sum_id, 1, MPI_INTEGER, size-1, msg, MPI_COMM_WORLD, status, ierr)
		write(*,*) "added up process ids: ", sum_id
	
	else
		! receive the current value of sum_id from the processor before
		call MPI_Recv(sum_id, 1, MPI_INTEGER, rank-1, msg, MPI_COMM_WORLD, status, ierr)
		
		if (rank < size-1) then
			! if it's not the last processor set the recipient to the next processor
			recp = rank+1
		else
			! if it's the last processor set the recipient to the first processor
			recp = 0
		
		! send the current sum of ids plus the processor id
		call MPI_Send(sum_id+rank, 1, MPI_INTEGER, recp, msg, MPI_COMM_WORLD, ierr)
		
		endif
	endif
	
	call MPI_FINALIZE(ierr)
	
	! RESULTS
	! 4 proc : 6
	! 8 proc : 28
	! 11 proc :	55
	
end program