program Poisson
	use mpi
	use initialize
	use run
	use communicate
	use finalize

	implicit none

	real(kind=8), dimension(:,:), pointer :: matrix, chunck	! Matrix, Teilmatrix
	real(kind=8), dimension(:), pointer :: frow, lrow, efrow, elrow	! Halo lines
	integer, parameter :: NDIM = 96	! Dimension der Matrix
	integer, parameter :: interlines = 11	! Anzahl Interlines
	integer, parameter :: NITER = 100000	! Anzahl Iterationen
	integer, dimension(:), pointer :: sendcounts, displacement
	integer, parameter :: master = 0	! Master Prozess
	integer :: iter, cdim	! Anzahl der ausgeführten Iterationen, Dimension des Chuncks
	integer :: mpi_err, mpi_rank, mpi_size
	integer :: mpi_req, status(MPI_STATUS_SIZE)
	
	! MPI Initialisierung
	call MPI_INIT(mpi_err)
	call MPI_COMM_RANK(MPI_COMM_WORLD,mpi_rank,mpi_err)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,mpi_size,mpi_err)
	
	! Allokation, Initialisierung, erstes Ausgeben der Matrix
	if (mpi_rank == master) then
		call createMatrix(matrix, NDIM, NDIM)
		call initializeMatrix(matrix)
		call outputMatrix(matrix, interlines, 0)
	end if
	
	! Festlegen der Aufteilung der Matrix in Teilmatrizen
	call initSequence(sendcounts, displacement, NDIM+1, mpi_size)

	! Erweitere chunck fuer Randzeilen, der erste chunck hat nur eine
	! untere Randzeile, der letzte nur eine obere Randzeile
	if (mpi_rank == master .or. mpi_rank == mpi_size-1) then
		cdim = sendcounts(mpi_rank+1) / (NDIM+1) - 1 + 1	! eine Randzeile
	else
		cdim = sendcounts(mpi_rank+1) / (NDIM+1) - 1 + 2	! zwei Randzeilen
	end if
	
	! Allokation der Teilmatrix
	call createMatrix(chunck, NDIM, cdim)

	! Verteilung der Matrix auf Teilmatrix
	if (mpi_rank == master) then
		call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_REAL8, &
		&		  chunck(:,0:cdim-1), sendcounts(mpi_rank+1), MPI_REAL8, &
		&		  master, MPI_COMM_WORLD, mpi_err)
	else if (mpi_rank == mpi_size-1) then
		call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_REAL8, &
		&		  chunck(:,1:), sendcounts(mpi_rank+1), MPI_REAL8, &
		&		  master, MPI_COMM_WORLD, mpi_err)
	else
		call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_REAL8, &
		&		  chunck(:,1:cdim-1), sendcounts(mpi_rank+1), MPI_REAL8, &
		&		  master, MPI_COMM_WORLD, mpi_err)
	end if
	
	! Iteration Jacobi Berechnung
	do iter=1,NITER
		call sendrecvHalo(master, NDIM, iter, chunck, cdim, mpi_err, mpi_rank, mpi_size, mpi_req, status)
		call calculate(chunck)

	end do

	if (mpi_rank == master) then
		call MPI_GATHERV(chunck(:,0:cdim-1), sendcounts(mpi_rank+1), MPI_REAL8, &
		&		 matrix, sendcounts, displacement, MPI_REAL8, &
		&		 master, MPI_COMM_WORLD, mpi_err)
	else if (mpi_rank == mpi_size-1) then
		call MPI_GATHERV(chunck(:,1:), sendcounts(mpi_rank+1), MPI_REAL8, &
		&		 matrix, sendcounts, displacement, MPI_REAL8, &
		&		 master, MPI_COMM_WORLD, mpi_err)
	else
		call MPI_GATHERV(chunck(:,1:cdim-1), sendcounts(mpi_rank+1), MPI_REAL8, &
		&		 matrix, sendcounts, displacement, MPI_REAL8, &
		&		 master, MPI_COMM_WORLD, mpi_err)
	end if
	
	call freeMatrix(chunck)
	
	if (mpi_rank == master) then
		call outputMatrix(matrix, interlines, iter-1)
		call freeMatrix(matrix)
	end if

	call MPI_FINALIZE(mpi_err)

end program Poisson
