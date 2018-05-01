! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 3
! Abgabe 02.05.2018
! Menken und Burgemeister

! Conway's 'Game of Life'
! http://en.wikipedia.org/wiki/Conway’s_Game_of_Life

module mod_lifecycle
	implicit none

contains

	! Subroutine developLife 
	! Entwicklung des Lebenszyklus anhand folgender Kriterien
	! - 	lebende Zelle (.true.) ueberlebt nur bei 2 oder 3 Nachbarn, 
	!	ansonsten stirbt diese (.false.)
	! -	tote Zelle (.false.) wird aktiviert (.true.) bei genau 3 
	!	lebenden Nachbarn (.true.)
	!	
	! field : logical array : Spielfeld
	subroutine developLife(field)
		logical, dimension(:,:), pointer, intent(inout) :: field
		integer, dimension(:,:), pointer :: neighbors
		integer :: xdim, ydim, xstep, ystep

		xdim = ubound(field, 1)
		ydim = ubound(field, 2)
				
		call countNeighbors(field, neighbors)

		do xstep = 1,xdim
			do ystep = 1,ydim
				if(field(xstep,ystep)) then
					field(xstep, ystep) = (.not. &
					&	((neighbors(xstep,ystep) < 2) &
					&	.or. &
					&	(neighbors(xstep,ystep) > 3)))
				else
					field(xstep, ystep) = &
					&	(neighbors(xstep,ystep) == 3)
				end if
			end do
		end do

		deallocate(neighbors)

	end subroutine
	
	! Subroutine countNeighbors
	! Zaehlt benachbarte lebende Zellen (.true.) auf dem gesamten Spielfeld
	!	
	! field : logical array : Spielfeld
	! neighbors : integer array : Nachbarn
	subroutine countNeighbors(field, neighbors)
		logical, dimension(:,:), pointer, intent(out) :: field
		logical, dimension(:,:), pointer :: boundless_field
		integer, dimension(:,:), pointer, intent(inout) :: neighbors
		integer :: xdim, ydim, xstep, ystep

		xdim = ubound(field, 1)
		ydim = ubound(field, 2)

		allocate(neighbors(xdim, ydim))
		allocate(boundless_field(0:xdim+1, 0:ydim+1))

		neighbors(:,:) = 0
		boundless_field(:,:) = .false.
		boundless_field(1:xdim,1:ydim) = field
		
		! setze zyklische Randbedingungen, sodass Figuren
		! im Spielfeld bleiben
		boundless_field(0,1:ydim) = field(xdim,1:ydim)
		boundless_field(xdim+1,1:ydim) = field(1,1:ydim)
		boundless_field(1:xdim,0) = field(1:xdim,ydim)
		boundless_field(1:xdim,ydim+1) = field(1:xdim,1)
		boundless_field(0,0) = field(xdim,ydim)
		boundless_field(0,ydim+1) = field(xdim,1)
		boundless_field(xdim+1,0) = field(1,ydim)
		boundless_field(xdim+1,ydim+1) = field(1,1)
		
		do xstep = 1,xdim
			do ystep = 1,ydim
				neighbors(xstep, ystep) = count( &
				&	boundless_field(xstep-1:xstep+1, &
				&			ystep-1:ystep+1))
				if(field(xstep, ystep)) then
					neighbors(xstep, ystep) = &
					&	neighbors(xstep, ystep) - 1
				end if
			end do
		end do

		deallocate(boundless_field)

	end subroutine
	
end module

