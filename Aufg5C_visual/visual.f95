program visualisation

	use mpi
	
	implicit none
	
	integer :: rank, size, ierr, status(MPI_STATUS_SIZE)
	character(14) :: msg1 = "Hello process!"
	integer :: msg2 = 34
	integer :: tag1 = 3, tag2 = 13
	integer :: irank

	! initialise MPI
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
	call MPI_COMM_SIZE (MPI_COMM_WORLD, size,ierr)
	print*, 'I am', rank, 'of', size

	! simple send and receive for process 0 and 1
	if (rank == 0) then
		call MPI_Send(msg1, 1, MPI_CHARACTER, 1, tag1, MPI_COMM_WORLD, ierr)
	
	else if (rank == 1) then
		call MPI_Recv(msg1, 1, MPI_CHARACTER, 0, tag1, MPI_COMM_WORLD, status, ierr)
		
	endif
	
	! barrier
	call MPI_BARRIER(MPI_COMM_WORLD, ierr)

	! broadcast integer
	call MPI_BCAST(msg2, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
	
	! barrier
	call MPI_BARRIER(MPI_COMM_WORLD, ierr)

	! send and receive similar to broadcast
	! process 0 sends to every other process
	! each other process just receives
	if (rank == 0) then
		do irank=1, size-1
			call MPI_Send(msg1, 1, MPI_CHARACTER, irank, tag2, MPI_COMM_WORLD, ierr)
		end do
	else
		call MPI_Recv(msg1, 1, MPI_CHARACTER, 0, tag2, MPI_COMM_WORLD, status, ierr)
	endif
	
	! barrier ?!
	call MPI_BARRIER(MPI_COMM_WORLD, ierr)
	
	call MPI_FINALIZE(ierr)
	
end program