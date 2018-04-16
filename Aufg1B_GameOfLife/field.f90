! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 1
! Abgabe 17.04.2018
! Menken und Burgemeister

! Conway's 'Game of Life'
! http://en.wikipedia.org/wiki/Conway’s_Game_of_Life

module mod_initializeField
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
	! Belegt Muster der Figuren Blinker, Toad und Beacon im Spielfeld 
	! mit dem Wert ’true’.
	! field : logical array : Spielfeld
	subroutine createFigures(field)
		logical, dimension(:,:), pointer, intent(inout) :: field
		logical, dimension(1:3,1:1) :: blinker = .false.
		logical, dimension(1:4,1:2) :: toad = .false.
		logical, dimension(1:6,1:6) :: beacon = .false.

		! setze Figur 'blinker'
		blinker(:,:) = .true.

		! setze Figur 'toad'
		toad(:,:) = .true.
		toad(1,1) = .false.
		toad(4,2) = .false.

		! setze Figur 'beacon'
		beacon(2:3,2) = .true.
		beacon(2,3) = .true.
		beacon(4:5,5) = .true.
		beacon(5,4) = .true.

		! erstelle Figuren im Spielfeld
		field(5:,3:) = blinker
		field(12:,8:) = toad
		field(22:,12:) = beacon 

	end subroutine
	
end module

