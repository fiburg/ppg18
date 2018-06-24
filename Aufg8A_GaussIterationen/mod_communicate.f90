! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 7
! Abgabe 19.06.2018
! Menken und Burgemeister

! Modul zur Kommunikation zwischen Prozessen
module communicate
	use mpi
	
	implicit none
	contains

	! Kommunikation der Halo-Line zwischen den Prozessen
	subroutine sendrecvHalo(chunck, frow, lrow, efrow, elrow, master, &
	&			NDIM, iter, cdim, mpi_err, mpi_rank, &
	&			mpi_size, status)
		implicit none
		real(kind=8), dimension(:,:), pointer, intent(inout) :: chunck
		real(kind=8), dimension(:), pointer, intent(inout) :: frow, lrow, efrow, elrow
		integer, intent(in) :: master, iter, cdim, NDIM
		integer, intent(in) :: mpi_err, mpi_rank, mpi_size, mpi_req, status(MPI_STATUS_SIZE)
		integer :: mpi_req
		
		frow(:) = chunck(:,1)
		lrow(:) = chunck(:,cdim-1)

		if (mpi_rank == master) then
			call MPI_ISEND(lrow, size(lrow), MPI_REAL8, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			
			call MPI_IRECV(elrow, size(elrow), MPI_REAL8, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			
			chunck(:,cdim) = elrow(:) ! untere Randzeile
		else if (mpi_rank == mpi_size-1) then
			call MPI_ISEND(frow, size(frow), MPI_REAL8, mpi_rank-1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			
			call MPI_IRECV(efrow, size(efrow), MPI_REAL8, mpi_rank-1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			
			chunck(:,0) = efrow(:) ! obere Randzeile
		else
			call MPI_ISEND(frow, size(frow), MPI_REAL8, mpi_rank-1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			call MPI_ISEND(lrow, size(lrow), MPI_REAL8, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			
			call MPI_IRECV(efrow, size(efrow), MPI_REAL8, mpi_rank-1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
			call MPI_IRECV(elrow, size(elrow), MPI_REAL8, mpi_rank+1, iter, &
			&	       MPI_COMM_WORLD, mpi_req, mpi_err)
			call MPI_WAIT(mpi_req, status, mpi_err)
	
			chunck(:,cdim) = elrow(:) ! obere Randzeile
			chunck(:,0) = efrow(:) ! untere Randzeile
		end if
	
	end subroutine

end module communicate
