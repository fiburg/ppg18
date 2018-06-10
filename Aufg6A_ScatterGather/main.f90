program main
	use mpi
	use initialize
	
	implicit none

	integer(kind=2), dimension(:,:), pointer :: matrix
	integer(kind=2), dimension(:,:), pointer :: chunck
	integer, parameter :: master = 0
	integer, parameter :: mdim = 24
	integer :: cdim, csize
	integer :: mpi_err, mpi_rank, mpi_size
	
	call MPI_INIT(ierr)
	call MPI_COMM_RANK(MPI_COMM_WORLD,mpi_rank,mpi_err)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,mpi_size,mpi_err)

	cdim = mdim / mpi_size
	csize = cdim * mdim
	call createMatrix(chunck, mdim, cdim)

	if (rank == master) then
		call createMatrix(matrix, mdim, mdim)
		call initializeMatrix(matrix, NDIM)	
	endif
	
	call MPI_SCATTER(matrix, csize, MPI_INTEGER, chunck, csize, MPI_INTEGER, &
			 master, MPI_COMM_WORLD, mpi_ierr)

	call operationSequence(chunck, mpi_rank)

	print*, sum(chunck)

	call MPI_GATHER(chunck, csize, MPI_INTEGER, matrix, csize, MPI_INTEGER, &
			master, MPI_COMM_WORLD, mpi_ierr)

	! final
	if (rank == master) deallocate(matrix)
	deallocate(chunck)

	call MPI_FINALIZE(mpi_err)

end program main
