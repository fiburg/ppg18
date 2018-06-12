program main
	use mpi
	use initialize
	use calculation
	
	implicit none

	integer, dimension(:,:), pointer :: matrix
	integer, dimension(:,:), pointer :: chunck
	integer, parameter :: master = 0
	integer, parameter :: mdim = 24
	integer :: i, cdim, csize, sum_chunck
	integer :: mpi_err, mpi_rank, mpi_size
	
	call MPI_INIT(mpi_err)
	call MPI_COMM_RANK(MPI_COMM_WORLD,mpi_rank,mpi_err)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,mpi_size,mpi_err)

	if (mpi_rank == master) then
		call createMatrix(matrix, mdim, mdim)
		call initializeMatrix(matrix)
	end if

	cdim = mdim / mpi_size
	csize = cdim * mdim
	
	call createMatrix(chunck, mdim, cdim)

	call MPI_SCATTER(matrix, csize, MPI_INTEGER, chunck, csize, MPI_INTEGER, master, MPI_COMM_WORLD, mpi_err)

	do i=1,24
		print*, chunck(i,:)
	end do
	print*, " "

	call operationSequence(chunck, mpi_rank)


	call MPI_REDUCE(sum(chunck), sum_chunck, 1, MPI_INTEGER, MPI_SUM, master, MPI_COMM_WORLD, mpi_err)
 
	print*, 'Die Summe der Teilmatrix des Prozesses ',mpi_rank,' betraegt ',sum(chunck)

	call MPI_GATHER(chunck, csize, MPI_INTEGER, matrix, csize, MPI_INTEGER, master, MPI_COMM_WORLD, mpi_err)

	! final
	if (mpi_rank == master) then
		print*, 'Die Gesamtsumme der Teilmatrizen betraegt ',sum_chunck
		print*, 'Die Summe der Gesamtmatrix betraegt ',sum(matrix)

		deallocate(matrix)
	end if

	deallocate(chunck)

	call MPI_FINALIZE(mpi_err)

end program main
