subroutine calibrpens(hw)

use params
implicit none

! calibration of soc sec contribution rates
real(prec),intent(in)::hw
real(prec)::tauinpt(751,1)
real(prec)::socsecinpt(2,nt)
real(prec)::af
integer::tc

if (optpens) then
	! input of contribution rates
	open(57,file='taupens.txt')
	read(57,*) tauinpt
	close(57)
	do tc=1,nt
		taups(tc)=hw*tauinpt(tc,1)
	end do
endif


end subroutine calibrpens