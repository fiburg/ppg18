! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 6
! Abgabe 12.06.2018
! Menken und Burgemeister

! MPI-Matrix Operationen mit Scatter und Gather
program main
	use mpi
	use initialize
	use calculation
	
	implicit none

	integer, dimension(:,:), pointer :: matrix
	integer, dimension(:,:), pointer :: chunck
	integer, dimension(:), pointer :: sendcounts, displacement
	integer, parameter :: master = 0
	integer, parameter :: mdim = 24
	integer :: i, cdim, csize, sum_chunck
	integer :: mpi_err, mpi_rank, mpi_size
	
	call MPI_INIT(mpi_err)
	call MPI_COMM_RANK(MPI_COMM_WORLD,mpi_rank,mpi_err)
	call MPI_COMM_SIZE(MPI_COMM_WORLD,mpi_size,mpi_err)

	if (mpi_rank == master) then
		! Allokation und Initialisierung der Matrix
		call createMatrix(matrix, mdim, mdim)
		call initializeMatrix(matrix)
	end if

	call initSequence(sendcounts, displacement, mdim, mpi_size)

	cdim = sendcounts(mpi_rank+1) / mdim

	! Allokation der Teilmatrix	
	call createMatrix(chunck, mdim, cdim)

	call MPI_SCATTERV(matrix, sendcounts, displacement, MPI_INTEGER, &
	&		  chunck, sendcounts(mpi_rank+1), MPI_INTEGER, &
	&		  master, MPI_COMM_WORLD, mpi_err)

	! Matrixoperation auf der Teilmatrix
	call operationSequence(chunck, mpi_rank)

	call MPI_REDUCE(sum(chunck), sum_chunck, 1, MPI_INTEGER, MPI_SUM, &
	&		master, MPI_COMM_WORLD, mpi_err)
 
	print*, 'Die Summe der Teilmatrix des Prozesses ',mpi_rank, &
	&	' betraegt ',sum(chunck)

	call MPI_GATHERV(chunck, sendcounts(mpi_rank+1), MPI_INTEGER, &
	&		matrix, sendcounts, displacement, MPI_INTEGER, &
	&		master, MPI_COMM_WORLD, mpi_err)


	if (mpi_rank == master) then
		print*, 'Die Gesamtsumme der Teilmatrizen betraegt ',sum_chunck
		print*, 'Die Summe der Gesamtmatrix betraegt ',sum(matrix)

		deallocate(matrix)
	end if

	deallocate(chunck)

	call MPI_FINALIZE(mpi_err)

end program main
