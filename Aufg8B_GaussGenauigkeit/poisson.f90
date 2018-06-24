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
	real(kind=8), parameter :: eps = 10.E-7	! geforderte Genauigkeit
	integer, parameter :: NDIM = 96	! Dimension der Matrix
	integer, parameter :: interlines = 11	! Anzahl Interlines
	integer, parameter :: NITER = 100000	! Anzahl Iterationen
	integer, dimension(:), pointer :: sendcounts, displacement
	integer, parameter :: master = 0	! Master Prozess
	integer :: iter, cdim	! Anzahl der ausgeführten Iterationen, Dimension des Chuncks
	integer :: mpi_err, mpi_rank, mpi_size
	integer :: mpi_mreq, mpi_lmreq, status(MPI_STATUS_SIZE)
	logical :: acc = .false., oacc = .false., fin = .false.	! Abbruchbedingung
	integer :: nruns = 0
	
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
	allocate(frow(0:NDIM), lrow(0:NDIM), efrow(0:NDIM), elrow(0:NDIM))
	frow(:) = 0
	lrow(:) = 0
	efrow(:) = 0
	elrow(:) = 0

	! Sende erste Zeile als erste Haloline zum Start der Berechnung 
	if (mpi_rank > master) then
		frow(:) = chunck(:,1)
		call MPI_ISEND(frow, size(frow), MPI_REAL8, mpi_rank-1, 1, &
		&	       MPI_COMM_WORLD, mpi_mreq, mpi_err)
		call MPI_WAIT(mpi_mreq, status, mpi_err)
	end if

	! Iteration Gauss Berechnung
	do iter=1,NITER
		if(mpi_rank > master) then
			call MPI_IRECV(fin, 1, MPI_LOGICAL, mpi_rank-1, iter, &
			&	       MPI_COMM_WORLD, mpi_mreq, mpi_err)
			call MPI_WAIT(mpi_mreq, status, mpi_err)
		end if

		if(mpi_rank < mpi_size-1 .and. iter > mpi_size - mpi_rank -1)then
			!print*,mpi_rank,":recvs:",iter,":from:",mpi_rank+1

			call MPI_IRECV(oacc, 1, MPI_LOGICAL, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_mreq, mpi_err)
			call MPI_WAIT(mpi_mreq, status, mpi_err)
		end if

		if(mpi_rank == master) then
			if(oacc) fin = .true.
		end if
	
		call recvHalo(chunck, efrow, elrow, master, NDIM, iter, cdim, &
		&	      mpi_err, mpi_rank, mpi_size, status)
		
		call calculate(chunck, eps, acc)
		
		if (mpi_rank == mpi_size-1) oacc = acc

		if (mpi_rank > master .and. iter > mpi_size - mpi_rank - 1)  then
			!print*, mpi_rank,":",(acc .and. oacc)
			call MPI_ISEND((acc .and. oacc), 1, MPI_LOGICAL, mpi_rank-1, iter+1, &
			&	       MPI_COMM_WORLD, mpi_mreq, mpi_err)
			!print*,mpi_rank,":sends:",iter+1,":to:",mpi_rank-1,":in:",iter
		end if

		if (mpi_rank < mpi_size-1) then
			call MPI_ISEND(fin, 1, MPI_LOGICAL, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_mreq, mpi_err)
		end if

		call sendHalo(chunck, efrow, elrow, master, NDIM, iter, cdim, &
		&	      mpi_err, mpi_rank, mpi_size, status)

		if (fin) then 
			print*,mpi_rank,":",iter
			exit
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
