! -------------------------------------------------------
subroutine calibrconsprof(fval,bettacoeffs2,nm)


!implicit none

use params
use alexutils
use paramsimsl



implicit none
real(prec),intent(out)::fval(2)
real(prec)::bettacoeffs2(2)
external house

integer::t0,t1,nnt,k0,k1,kc,tc,pc,jccc,jcc,jc
integer,parameter::nnj=j1dat-j0dat+1
integer,parameter::ncj=j1cdat-j0cdat+1
real(prec)::Yvec(nnj),Xmat(nnj,np-1),Ycvec(ncj-1),Ycdatvec(ncj-1),Xcmat(ncj-1,npc-1),SSE,SST
integer,parameter::intcep=1
real(prec)::polchk(np),polccons(npc),polcconsdat(npc)
real(prec)::avgcons(ncj),avgconsdat(ncj)
integer::mc,nm,ib
real(prec)::loopbettas(2)=(/0.5, 1.0/) 


t0 = tcal0
t1 = tcal1
nnt=t1-t0-1.0

if (optbettawf==2) then
	do ib=1,2
		bettacoeffs(1)=bettacoeffs2(1)*loopbettas(ib)
		bettacoeffs(2)=bettacoeffs2(2)*loopbettas(ib)
		call house(1,nt,1)
	end do
else
	call house(1,nt,1)
endif

! regression for consumption growth
avgcons(:)=0.0
do jc=1,ncj
	! base calibration on cohorts born in calibration period
	k0=tcal0 !t0
	k1=tcal1 !t1
	jccc=j0cdat-age0+jc
	avgcons(jc)=sum( cons_k(k0:k1,1,jccc) )/nnt 
	avgconsdat(jc)=consdat(jccc)
end do
do jc=1,ncj-1
	Ycvec(jc)=avgcons(jc+1)/avgcons(jc)
	Ycdatvec(jc)=avgconsdat(jc+1)/avgconsdat(jc)
	jcc=j0cdat+jc		! model age, actual age is jcc-1
	Xcmat(jc,1)=jcc-1
	do pc=3,npc
		Xcmat(jc,pc-1)=Xcmat(jc,pc-2)*Xcmat(jc,1)
	end do
end do
! correction of growth rate at retirement point
if (tetta>1.0) then
	jcc=jr-(j0cdat-age0+1)
	Ycvec(jcc)=0.5*(Ycvec(jcc+1)+Ycvec(jcc-1))
	Ycdatvec(jcc)=0.5*(Ycdatvec(jcc+1)+Ycdatvec(jcc-1))	
endif
call drlse(ncj-1, Ycdatvec, npc-1, Xcmat, ncj-1, intcep, polcconsdat, SST, SSE)
call drlse(ncj-1, Ycvec, npc-1, Xcmat, ncj-1, intcep, polccons, SST, SSE)


! simulated moments and function values
fval(1)=(polcconsdat(1)-polccons(1))/polcconsdat(1)
fval(2)=(polcconsdat(2)-polccons(2))/polcconsdat(2)


! print and store
open(unit=99,file='bettaconscalibrhhonly.txt')
print*, ' '
do mc=1,nm
	print*, 'current function value ', mc, 'is ', fval(mc)
	print*, 'current coefficient estimate ', mc, 'is: ', bettacoeffs2(mc)
	write(99,'(2f24.14)') bettacoeffs2(mc)
end do
print*, ' '
close(99)


end subroutine calibrconsprof
! -------------------------------------------------------