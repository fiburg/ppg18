! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 7
! Abgabe 19.06.2018
! Menken und Burgemeister

program Poisson
	use mpi
	use initialize
	use run
	use communicate
	use finalize

	implicit none

	real(kind=8), dimension(:,:), pointer :: matrix, chunck
	real(kind=8), dimension(:), pointer :: frow, lrow, efrow, elrow	! Halo
	real(kind=8), parameter :: eps = 10.E-7	! geforderte Genauigkeit	
	integer, parameter :: NDIM = 96	! Dimension der Matrix
	integer, parameter :: interlines = 11	! Anzahl Interlines
	integer, parameter :: NITER = 100000	! Anzahl Iterationen
	integer, parameter :: master = 0	! Master Prozess
	integer, dimension(:), pointer :: sendcounts, displacement
	integer :: iter, cdim	! ausgeführten Iterationen, Dimension Chuncks
	integer :: mpi_err, mpi_rank, mpi_size
	integer :: mpi_req, status(MPI_STATUS_SIZE)
	logical :: acc = .false., fin = .false.	! Abbruchbedingung
	
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
		! eine Randzeile
		cdim = sendcounts(mpi_rank+1) / (NDIM+1) - 1 + 1
	else
		! zwei Randzeilen
		cdim = sendcounts(mpi_rank+1) / (NDIM+1) - 1 + 2
	end if
	
	! Allokation der Teilmatrix
	call createMatrix(chunck, NDIM, cdim)

	! Verteilung der Matrix auf Teilmatrix
	if (mpi_rank == master) then
		call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_REAL8, &
		&		  chunck(:,0:cdim-1), sendcounts(mpi_rank+1), &
		&		  MPI_REAL8, master, MPI_COMM_WORLD, mpi_err)
	else if (mpi_rank == mpi_size-1) then
		call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_REAL8, &
		&		  chunck(:,1:), sendcounts(mpi_rank+1), &
		&		  MPI_REAL8, master, MPI_COMM_WORLD, mpi_err)
	else
		call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_REAL8, &
		&		  chunck(:,1:cdim-1), sendcounts(mpi_rank+1), &
		&		  MPI_REAL8, master, MPI_COMM_WORLD, mpi_err)
	end if
	
	! Iteration Jacobi Berechnung
	do iter=1,NITER
		call sendrecvHalo(master, NDIM, iter, chunck, cdim, mpi_err, &
		&		  mpi_rank, mpi_size, mpi_req, status)
		call calculate(chunck, eps, acc)

		! Abbruch, wenn alle Prozesse Genauigkeit erreicht haben
		call MPI_ALLREDUCE(acc, fin, 1, MPI_LOGICAL, MPI_LAND, &
		&		   MPI_COMM_WORLD, mpi_err)
		if (fin) exit 
	end do

	! Zusammensetzen der Matrix aus Teilmatrizen
	if (mpi_rank == master) then
		call MPI_GATHERV(chunck(:,0:cdim-1), sendcounts(mpi_rank+1), &
		&		 MPI_REAL8, matrix, sendcounts, displacement, &
		&		 MPI_REAL8, master, MPI_COMM_WORLD, mpi_err)
	else if (mpi_rank == mpi_size-1) then
		call MPI_GATHERV(chunck(:,1:), sendcounts(mpi_rank+1), &
		&		 MPI_REAL8, matrix, sendcounts, displacement, &
		&		 MPI_REAL8, master, MPI_COMM_WORLD, mpi_err)
	else
		call MPI_GATHERV(chunck(:,1:cdim-1), sendcounts(mpi_rank+1), &
		&		 MPI_REAL8, matrix, sendcounts, displacement, &
		&		 MPI_REAL8, master, MPI_COMM_WORLD, mpi_err)
	end if
	
	call freeMatrix(chunck)
	
	if (mpi_rank == master) then
		! Notwendiges Dekrement bei Abbruch nach Iterationen
		if (iter > NITER) iter = iter-1
		call outputMatrix(matrix, interlines, iter)
		call freeMatrix(matrix)
	end if

	call MPI_FINALIZE(mpi_err)

end program Poisson
