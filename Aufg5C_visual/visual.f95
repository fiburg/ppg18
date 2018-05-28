! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 5
! Abgabe 29.05.2018
! Menken und Burgemeister

! Visualisation of MPI commands with Score-P and Vampir
program visualisation

	use mpi
	
	implicit none
	
	integer :: rank, size, ierr, status(MPI_STATUS_SIZE)
	character(14) :: msg1 = "Hello process!"
	integer :: msg2 = 34
	integer :: tag1 = 3, tag2 = 13
	integer :: irank
	integer :: recp	! recipient of MPI sending

	! initialise MPI
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)
	call MPI_COMM_SIZE (MPI_COMM_WORLD, size,ierr)

	! barrier for better overview in vampir
	call MPI_BARRIER(MPI_COMM_WORLD, ierr)

	! 1. SEND and RECEIVE
	! each process sends and receives a message from another process, 
	! similiar to ring
	if (rank == 0) then
		call MPI_SEND(msg1, 1, MPI_CHARACTER, rank+1, tag1, &
		&	      MPI_COMM_WORLD, ierr)

		call MPI_RECV(msg1, 1, MPI_INTEGER, size-1, tag1, &
			      MPI_COMM_WORLD, status, ierr)
	
	else
		call MPI_RECV(msg1, 1, MPI_CHARACTER, rank-1, tag1, &
			      MPI_COMM_WORLD, status, ierr)

		if (rank < size-1) then
			recp = rank+1
		else
			recp = 0
		endif
		
		call MPI_SEND(msg1, 1, MPI_CHARACTER, recp, tag1, &
		&	      MPI_COMM_WORLD, ierr)
	endif
	
	! barrier
	call MPI_BARRIER(MPI_COMM_WORLD, ierr)

	! 2. BROADCAST of one integer
	call MPI_BCAST(msg2, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
	
	! barrier
	call MPI_BARRIER(MPI_COMM_WORLD, ierr)

	! 3. BROADCAST with SEND and RECEIVE
	! send and receive similar to broadcast
	! process 0 sends to every other process
	! each other process just receives
	if (rank == 0) then
		do irank=1, size-1
			call MPI_SEND(msg2, 1, MPI_INTEGER, irank, tag2, &
			&	      MPI_COMM_WORLD, ierr)
		end do
	else
		call MPI_RECV(msg2, 1, MPI_INTEGER, 0, tag2, &
		&	      MPI_COMM_WORLD, status, ierr)
	endif
	
	! barrier
	call MPI_BARRIER(MPI_COMM_WORLD, ierr)
	
	call MPI_FINALIZE(ierr)
	
end program
