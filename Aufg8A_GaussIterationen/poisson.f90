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

	real(kind=8), dimension(:,:), pointer :: matrix, chunck	! Matrix, Teilmatrix
	real(kind=8), dimension(:), pointer :: frow, lrow, efrow, elrow	! Halo lines
	integer, parameter :: NDIM = 96	! Dimension der Matrix
	integer, parameter :: interlines = 11	! Anzahl Interlines
	integer, parameter :: NITER = 100000	! Anzahl Iterationen
	integer, dimension(:), pointer :: sendcounts, displacement
	integer, parameter :: master = 0	! Master Prozess
	integer :: iter, cdim	! Anzahl der ausgeführten Iterationen, Dimension des Chuncks
	integer :: mpi_err, mpi_rank, mpi_size
	integer :: mpi_req, mpi_lreq, status(MPI_STATUS_SIZE)
	
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
	
	! Allokation der Randzeilen zur Kommunikation
	allocate(frow(0:NDIM))
	allocate(lrow(0:NDIM))
	allocate(efrow(0:NDIM))
	allocate(elrow(0:NDIM))
	
	! Sende erste Zeile als erste Haloline zum Start der Berechnung 
	if (mpi_rank > master) then
		frow(:) = chunck(:,1)
		call MPI_ISEND(frow, size(frow), MPI_REAL8, mpi_rank-1, 1, &
		&	       MPI_COMM_WORLD, mpi_req, mpi_err)
		call MPI_WAIT(mpi_req, status, mpi_err)
	end if

	! Iteration Gauss Berechnung
	do iter=1,NITER
		if (mpi_rank == master) then
			call MPI_IRECV(elrow, size(elrow), MPI_REAL8, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			
			chunck(:,cdim) = elrow(:) ! untere Randzeile
		else if (mpi_rank == mpi_size-1) then
			call MPI_IRECV(efrow, size(efrow), MPI_REAL8, mpi_rank-1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			
			chunck(:,0) = efrow(:) ! obere Randzeile
		else
			call MPI_IRECV(efrow, size(efrow), MPI_REAL8, mpi_rank-1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_IRECV(elrow, size(elrow), MPI_REAL8, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_lreq, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			call MPI_WAIT(mpi_lreq, status, mpi_err)
			
			chunck(:,cdim) = elrow(:) ! obere Randzeile
			chunck(:,0) = efrow(:) ! untere Randzeile
		end if
		
		call calculate(chunck)

		frow(:) = chunck(:,1)
		lrow(:) = chunck(:,cdim-1)
		
		if (mpi_rank == master) then
			call MPI_ISEND(lrow, size(lrow), MPI_REAL8, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
		else if (mpi_rank == mpi_size-1) then
			call MPI_ISEND(frow, size(frow), MPI_REAL8, mpi_rank-1, iter+1, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
		else
			call MPI_ISEND(frow, size(frow), MPI_REAL8, mpi_rank-1, iter+1, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_ISEND(lrow, size(lrow), MPI_REAL8, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_lreq, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			call MPI_WAIT(mpi_lreq, status, mpi_err)
		end if
	end do
	
	! Deallokation der Randzeilen der Kommunikation
	deallocate(elrow, efrow, frow, lrow)
	
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
