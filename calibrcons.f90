subroutine calibrcons

! calibration of human capital function

use params
implicit none

real(prec)::consinpt(5,j1cdat-j0cdat+1)
integer::jc,jcc

! input of coefficients
open(51,file='nondur_profiles.txt')
read(51,*) consinpt
close(51)

do jc=j0cdat,j1cdat
	jcc=jc-age0+1
	consdat(jcc)=consinpt(4,jc-j0cdat+1)
end do

! simplification: irrelevant below b/c information is not used
jcc=j0cdat-age0+1
consdat(1:jcc)=consdat(jcc)
jcc=j1cdat-age0+1
consdat(jcc:nj)=consdat(jcc)

end subroutine calibrcons