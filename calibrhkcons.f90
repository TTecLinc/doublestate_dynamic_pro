subroutine calibrhkcons

! calibration of human capital function

use params
implicit none

real(prec)::coeffhkinpt(np,1),coeffconsinpt(2,npc),consdatinpt(2,j1cdat-j0cdat+1),bettainpt(1,2)
! real(prec)::consinpt(age1cdat-age0cdat+1,2)
integer::pc,jc,jcc,jcl,jcu

! input of coefficients for earnings
open(51,file='polcoeffhk.txt')
read(51,*) coeffhkinpt
close(51)

do pc=1,np
	polchkdat(pc)=coeffhkinpt(pc,1)
end do

! input of consumption data
open(51,file='consdata.txt')
read(51,*) consdatinpt
close(51)

! read data
jcl=j0cdat-age0+1
jcu=j1cdat-age0+1
do jc=jcl,jcu
	jcc=jc-jcl+1
	consdat(jc)=dfcons*consdatinpt(1,jcc)+(1.0-dfcons)*consdatinpt(2,jcc)
end do


end subroutine calibrhkcons