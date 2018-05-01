! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 3
! Abgabe 02.05.2018
! Menken und Burgemeister

! Conway's 'Game of Life'
! http://en.wikipedia.org/wiki/Conway’s_Game_of_Life

module mod_initializeField
	use mod_figure
	implicit none


contains
	
	! Subroutine createField 
	! Initialisierung des Spielfelds der Groesse 20 x 30
	! field : logical array : Spielfeld
	subroutine createField(field)
		integer, parameter :: XDIM = 30, YDIM = 20	! field size
		logical, dimension(:,:), pointer, intent(out) :: field

		allocate(field(1:XDIM,1:YDIM))

		field(:,:) = .false.

	end subroutine
	
	! Subroutine createFigures
	! Belegt Muster der Figuren im Spielfeld mit dem Wert ’true’.
	! field : logical array : Spielfeld
	subroutine createFigures(field)
		logical, dimension(:,:), pointer, intent(inout) :: field
		logical, dimension(:,:), pointer :: figure
		integer :: startX = 5, startY = 5
		integer :: num_figure, io

		! Figurauswahl
		write(*,*) 'Waehle eine Figur durch Eingabe eines Integers:'
		write(*,*) 'Glider (1), Lightweight spaceship (2),'
		write(*,*) 'Blinker (3), Toad (4) oder Beacon (5).'
		write(*,*) 'Eine andere Eingabe fuehrt zum Abbruch...'
		read(*,*,iostat=io) num_figure
		if(io /= 0) then
			write(*,*) 'Abbruch: kein Integer'
			deallocate(field)
			stop
		end if

		! Initialisiere und deklariere Figur
		select case(num_figure)
			case(1)
				call makeGlider(figure)
			case(2)
				call makeLwss(figure)
			case(3)
				call makeBlinker(figure)
			case(4)
				call makeToad(figure)
			case(5)
				call makeBeacon(figure)
			case default
				write(*,*) 'Abbruch: Keine Figur ausgewaehlt'
				deallocate(field)
				stop
		
		end select

		! erstelle Figur im Spielfeld
		field(startX:,startY:) = figure

		deallocate(figure)

	end subroutine

end module

