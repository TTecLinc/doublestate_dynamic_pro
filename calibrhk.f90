subroutine calibrhk

! calibration of human capital function

use params
implicit none

real(prec)::coeffinpt(np,1)
integer::cp

! input of coefficients
open(51,file='polcoeff.txt')
read(51,*) coeffinpt
close(51)

do cp=1,np
	polcdat(cp)=coeffinpt(cp,1)
end do

end subroutine calibrhk