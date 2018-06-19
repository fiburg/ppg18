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
	subroutine sendrecvHalo(master, NDIM, iter, chunck, cdim, mpi_err, mpi_rank, mpi_size, mpi_req, status)
		implicit none
		real(kind=8), dimension(:,:), pointer, intent(inout) :: chunck
		real(kind=8), dimension(:), pointer :: frow, lrow, efrow, elrow
		integer, intent(in) :: master, iter, cdim, NDIM
		integer, intent(in) :: mpi_err, mpi_rank, mpi_size, mpi_req, status(MPI_STATUS_SIZE)
		
		allocate(frow(0:NDIM))
		allocate(lrow(0:NDIM))
		allocate(efrow(0:NDIM))
		allocate(elrow(0:NDIM))
	
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
		
		deallocate(elrow)
		deallocate(efrow)
		deallocate(frow)
		deallocate(lrow)
	
	end subroutine

end module communicate
