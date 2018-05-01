! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 3
! Abgabe 02.05.2018
! Menken und Burgemeister

! Conway's 'Game of Life'
! http://en.wikipedia.org/wiki/Conway’s_Game_of_Life

program aufgabe1b
	use mod_initializeField
	use mod_lifecycle
	use mod_utilities
	implicit none

	integer :: outputUnit = 6, timestep
	logical, dimension(:,:), pointer :: field

	! printTwoDlogical benoetigt UTF-8 character set fuer stdout
	open(outputUnit, encoding='UTF-8')    
	
	! erzeuge Spielfeld
	call createField(field)

	! erzeuge Figuren
	call createFigures(field)

	! gebe Intial-Spielfeld aus
	call printTwoDLogical(outputUnit, field)

	! iteriere ueber 160 Zeitschritte und stelle Figur-Lebenszyklen dar
	do timestep = 1,160
		call developLife(field)

		call printTwoDLogical(outputUnit, field)

	end do

	deallocate(field)

end program

