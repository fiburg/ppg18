program Poisson
	use mpi
	use initialize
	use run
	use finalize

	implicit none

	real(kind=8), dimension(:,:), pointer :: matrix, chunck
	real(kind=8), dimension(:), pointer :: frow, lrow, efrow, elrow
	integer, dimension(:), pointer :: sendcounts, displacement
	integer, parameter :: NDIM = 184
	integer, parameter :: interlines = 22
	integer, parameter :: NITER = 400000	! Anzahl Iterationen
	integer, parameter :: master = 0	
	integer :: iter, cdim
	integer :: mpi_err, mpi_rank, mpi_size
	integer :: mpi_req, status(MPI_STATUS_SIZE)

	call MPI_INIT(mpi_err)
	call MPI_COMM_RANK(MPI_COMM_WORLD,mpi_rank,mpi_err)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,mpi_size,mpi_err)

	if (mpi_rank == master) then
		call createMatrix(matrix, NDIM, NDIM)
		call initializeMatrix(matrix)
		call outputMatrix(matrix, interlines, 0)
		print*, matrix(:,0)

	end if

	call initSequence(sendcounts, displacement, NDIM+1, mpi_size)

	! Erweitere chunck fuer Randzeilen, der erste chunck hat nur eine
	! untere Randzeile, der letzte nur eine obere Randzeile
	if (mpi_rank == master .or. mpi_rank == mpi_size-1) then
		cdim = sendcounts(mpi_rank+1) / NDIM - 1 + 1
	else
		cdim = sendcounts(mpi_rank+1) / NDIM - 1 + 2
	end if
	
	call createMatrix(chunck, NDIM, cdim)

	! Verteilung der Matrix auf chuncks
	if (mpi_rank == master) then
		call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_INTEGER, &
		&		  chunck(:,0:cdim-1), sendcounts(mpi_rank+1), MPI_INTEGER, &
		&		  master, MPI_COMM_WORLD, mpi_err)
	else if (mpi_rank == mpi_size-1) then
		call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_INTEGER, &
		&		  chunck(:,1:), sendcounts(mpi_rank+1), MPI_INTEGER, &
		&		  master, MPI_COMM_WORLD, mpi_err)
	else
		call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_INTEGER, &
		&		  chunck(:,1:cdim-1), sendcounts(mpi_rank+1), MPI_INTEGER, &
		&		  master, MPI_COMM_WORLD, mpi_err)
	end if

	allocate(frow(0:NDIM))
	allocate(lrow(0:NDIM))
	allocate(efrow(0:NDIM))
	allocate(elrow(0:NDIM))


	do iter=1,2!NITER

		frow(:) = chunck(:,1)
		lrow(:) = chunck(:,cdim-1)

		print*,shape(chunck)

		if (mpi_rank == master) then
			call MPI_ISEND(lrow, size(lrow), MPI_REAL, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			
			call MPI_IRECV(elrow, size(elrow), MPI_REAL, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			
			chunck(:,cdim) = elrow(:)
		else if (mpi_rank == mpi_size-1) then
			call MPI_ISEND(frow, size(frow), MPI_REAL, mpi_rank-1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			
			call MPI_IRECV(efrow, size(efrow), MPI_REAL, mpi_rank-1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			
			chunck(:,0) = efrow(:)
		else
			call MPI_ISEND(frow, size(frow), MPI_REAL, mpi_rank-1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			call MPI_ISEND(lrow, size(lrow), MPI_REAL, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			
			call MPI_IRECV(efrow, size(efrow), MPI_REAL, mpi_rank-1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			call MPI_IRECV(elrow, size(elrow), MPI_REAL, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
	
			chunck(:,cdim) = elrow(:)
			chunck(:,0) = efrow(:)
		end if

		call calculate(chunck)

	end do

	deallocate(elrow)
	deallocate(efrow)
	deallocate(frow)
	deallocate(lrow)

	

	if (mpi_rank == master) then
		call MPI_GATHERV(chunck(:,0:cdim-1), sendcounts(mpi_rank+1), MPI_INTEGER, &
		&		 matrix, sendcounts, displacement, MPI_INTEGER, &
		&		 master, MPI_COMM_WORLD, mpi_err)
	else if (mpi_rank == mpi_size-1) then
		call MPI_GATHERV(chunck(:,1:), sendcounts(mpi_rank+1), MPI_INTEGER, &
		&		 matrix, sendcounts, displacement, MPI_INTEGER, &
		&		 master, MPI_COMM_WORLD, mpi_err)
	else
		call MPI_GATHERV(chunck(:,1:cdim-1), sendcounts(mpi_rank+1), MPI_INTEGER, &
		&		 matrix, sendcounts, displacement, MPI_INTEGER, &
		&		 master, MPI_COMM_WORLD, mpi_err)
	end if
	
	if (mpi_rank == master) then

		print*, matrix(:,0)
		call outputMatrix(matrix, interlines, iter-1)
		call freeMatrix(matrix)
	end if

	call MPI_FINALIZE(mpi_err)

end program Poisson
