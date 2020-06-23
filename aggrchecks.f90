subroutine aggrchecks

use params
implicit none

integer::tc,sc
real(prec)::diff,merc,meec,mepc
real(prec),parameter::epsi=1.0e-02

merc=0.0
meec=0.0
mepc=0.0
do tc=1,nt
    ! test ressource constraint
    diff=(aggprod(tc)+agglstranf(tc)-(agginv(tc)+aggcons(tc)+agggcons(tc)))/aggprod(tc)
    if ( abs(diff)>merc ) then
		merc=diff
	endif

    ! test Euler condition
    diff=(aggprod(tc)-(mpk(tc)*aggcap(tc)+At(tc)*wage(tc)*aggeffhrs(tc)))/aggprod(tc)
    if ( abs(diff)>meec ) then
		meec=diff
	endif

	! test pension budget
	diff=(taups(tc)*wage(tc)*aggeffhrs(tc)-pens(tc))/pens(tc)
    if ( abs(diff)>mepc ) then
		mepc=diff
	endif

end do

print*, 'max. resource constraint violation is ', merc
print*, 'max. Euler theorem error is ', meec
print*, 'max. pension budget violation is ', mepc

end subroutine aggrchecks