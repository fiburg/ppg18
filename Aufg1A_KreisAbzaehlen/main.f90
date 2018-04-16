! Paralleles Programmieren für Geowissenschaftler im SS 2018
! Uebungsblatt 1
! Abgabe 17.04.2018
! Menken und Burgemeister

! Von 32 Personen wird ein Kreis gebildet. Eine Person wird als Nummer 1 
! identifiziert und dann wird von dieser Person ab abgezählt. Dabei verläßt 
! jede dritte Person den Kreis und der Kreis wird ohne die fehlende Personen 
! wieder geschlossen und ab der nächsten Person wieder ab 1 weitergezählt bis
! 3. Die Reihenfolge der Personen, die den Kreis verlassen, in Bezug auf die 
! ursprünglichen Aufstellung im Kreis, wird im Terminal ausgegeben.

program aufgabe1a
	implicit none
	integer, parameter :: NPERS = 32		! Anzahl Personen
	integer	:: ipers = 1, icount = 0		! Schleifen-Indices
	logical, dimension(NPERS) :: lincirc = .true.	! Gruppenzugehoerigkeit

	do while (any(lincirc))
		! Wenn die Person noch im Kreis ist, wird hochgezaehlt
		if (lincirc(ipers)) icount = icount + 1

		! Jede dritte Person wird aus dem Kreis ausgeschlossen
		if (icount == 3) then
			icount = 0
			lincirc(ipers) = .false.
			write(*,*) ipers

		end if

		! Es wird ueber die Personen iteriert
		ipers = ipers + 1

		! Nach abzaehlen der letzten Person wird am Anfang begonnen
		if (ipers > NPERS) ipers = 1

	end do

end program aufgabe1a
