program Poisson
	use mpi
	use initialize
	use run
	use finalize

	implicit none

	real(kind=8), dimension(:,:), pointer :: matrix, chunck
	integer, dimension(:), pointer :: sendcounts, displacement
	integer, parameter :: NDIM = 184
	integer, parameter :: interlines = 22
	integer, parameter :: NITER = 400000	! Anzahl Iterationen
	integer, parameter :: master = 0	
	integer :: iter, cdim
	integer :: mpi_err, mpi_rank, mpi_size

	call MPI_INIT(mpi_err)
	call MPI_COMM_RANK(MPI_COMM_WORLD,mpi_rank,mpi_err)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,mpi_size,mpi_err)

	if (mpi_rank == master) then
		call createMatrix(matrix, NDIM, NDIM)
		call initializeMatrix(matrix)
		call outputMatrix(matrix, interlines, 0)
	end if

	call initSequence(sendcounts, displacement, NDIM+1, 5)!mpi_size)

	if (mpi_rank == master .or. mpi_rank == mpi_size) then
		cdim = sendcounts(mpi_rank+1) / NDIM - 1 + 1
	else
		cdim = sendcounts(mpi_rank+1) / NDIM - 1 + 2
	end if
	
	call createMatrix(chunck, NDIM, cdim)

	print*,shape(chunck)
	
	!call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_INTEGER, &
	!&		  chunck, sendcounts(mpi_rank+1), MPI_INTEGER, &
	!&		  master, MPI_COMM_WORLD, mpi_err)	

	do iter=1,2!NITER
		call calculate(chunck)
	end do

	!call MPI_GATHERV(chunck, sendcounts(mpi_rank+1), MPI_INTEGER, &
	!&		matrix, sendcounts, displacement, MPI_INTEGER, &
	!&		master, MPI_COMM_WORLD, mpi_err)

	call outputMatrix(matrix, interlines, iter-1)
	call freeMatrix(matrix)

	call MPI_FINALIZE(mpi_err)

end program Poisson
