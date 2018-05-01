! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 3
! Abgabe 02.05.2018
! Menken und Burgemeister

! Conway's 'Game of Life'
! http://en.wikipedia.org/wiki/Conway’s_Game_of_Life

! Modul zur Erstellung der Figuren: 
! dynamisch: Glider, Lightweight spaceship (LWSS)
! oszillierend: Blinker, Toad, Beacon
module mod_figure
	implicit none

contains

	! Subroutine makeGlider
	! Initialisiert und deklariert die Figur Glider.
	! glider : logical array : Figur
	subroutine makeGlider(glider)
		logical, dimension(:,:), pointer, intent(out) :: glider
		allocate(glider(1:3,1:3))
		glider = .true.
		glider(2, 1) = .false.
		glider(1, 2:3) = .false.
		glider(3, 3) = .false.

	end subroutine

	! Subroutine makeLwss
	! Initialisiert und deklariert die Figur Lightweight spaceship (LWSS).
	! lwss : logical array : Figur
	subroutine makeLwss(lwss)
		logical, dimension(:,:), pointer, intent(out) :: lwss
		allocate(lwss(1:5,1:4))
		lwss = .true.
		lwss(1, 2) = .false.
		lwss(1, 4) = .false.
		lwss(2:3, 1:3) = .false.
		lwss(4, 2:3) = .false.
		lwss(5, 1) = .false.

	end subroutine

	! Subroutine makeBlinker
	! Initialisiert und deklariert die Figur Blinker.
	! blinker : logical array : Figur
	subroutine makeBlinker(blinker)
		logical, dimension(:,:), pointer, intent(out) :: blinker
		allocate(blinker(1:3,1:1))
		blinker(:,:) = .true.
	
	end subroutine

	! Subroutine makeToad
	! Initialisiert und deklariert die Figur Toad.
	! toad : logical array : Figur
	subroutine makeToad(toad)
		logical, dimension(:,:), pointer, intent(out) :: toad
		allocate(toad(1:4,1:2))
		toad(:,:) = .true.
		toad(1,1) = .false.
		toad(4,2) = .false.

	end subroutine

	! Subroutine makeBeacon
	! Initialisiert und deklariert die Figur Beacon.
	! beacon : logical array : Figur
	subroutine makeBeacon(beacon)
		logical, dimension(:,:), pointer, intent(out) :: beacon
		allocate(beacon(1:6,1:6))
		beacon(:,:) = .false.
		beacon(2:3,2) = .true.
		beacon(2,3) = .true.
		beacon(4:5,5) = .true.
		beacon(5,4) = .true.

	end subroutine
	
end module

