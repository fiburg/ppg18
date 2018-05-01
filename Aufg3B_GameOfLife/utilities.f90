! (c) 2012,2013 Körner, Hübbe
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.

module mod_utilities
	implicit none
	private utf32
	!This character kind allows us to access the entire unicode character set. For details, see the wikipedia article for UTF-32.
	integer, parameter :: utf32=selected_char_kind('ISO_10646')
contains

	function intToStr(a)
		character(len=11) :: intToStr
		integer, intent(in) :: a
		write(intToStr, '(i11)') a
		intToStr = trim(adjustl(intToStr))
	end function

	subroutine waste_cycles(iterations)
		integer(kind=8), intent(in) :: iterations
		integer(kind=8), save :: trash
		integer(kind=8) :: i
		
		do i = 1, iterations
			trash = trash + i
		end do
	end subroutine

	subroutine portable_sleep(secondsToWait)
		implicit none
		real :: secondsToWait, elapsedTime ! time interval in seconds
		integer :: countStart, countEnd, countRate

		call system_clock(countStart, countRate)
		if (countRate /= 0) then
			do
				call system_clock(countEnd)
				elapsedTime = (countEnd - countStart)/float(countRate)
				if (elapsedTime > secondsToWait) exit
			end do
		else
			! No system clock available if (countRate == 0). Fall back to a very simple delay method.
			call waste_cycles(int(1000*1000*1000*secondsToWait, kind=8))
		end if
	end subroutine

	!The outputUnit must be opened with UTF-8 as the encoding!
	subroutine printTwoDLogical(outputUnit, matrix)
		integer, intent(in) :: outputUnit
		logical, dimension(:,:), intent(in) :: matrix
		integer :: width, height
		character(kind=utf32, len=1), dimension(:,:), allocatable :: characterMatrix
		! z'1b' hexadezimale Zahl, dezimaler Wert ist 27 
		! char(int(z'00B7'), utf32) ist das Zeichen mit der hexad. Nummer 00B7 
		! des Zeichensatzes 'ISO_10646'
		! 00B7 = middle dot, 2588 = full block
		character(kind=utf32, len=1) :: blockChar = char(int(z'2588'), utf32), emptyChar = char(int(z'00B7'), utf32)
		character(len=22) :: formatString
		
		!Create the character matrix.
		width = ubound(matrix, 1)
		height = ubound(matrix, 2)
		allocate(characterMatrix(1:width, 1:height))
		where (matrix)
			characterMatrix = blockChar
		else where
			characterMatrix = emptyChar
		end where

		!Write it out.
		formatString = '(' // intToStr(width) // 'a)'
		write(outputUnit, formatString) characterMatrix
		formatString = '(' // intToStr(width) // '("="))'
		write(outputUnit, formatString)
		
		!Waste some time.
		call portable_sleep(0.3)
		deallocate(characterMatrix)
	end subroutine

end module
