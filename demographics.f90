subroutine demographics(hwght)

use params
implicit none

real(prec),intent(in)::hwght
real(prec),dimension(104,751)::usp,tempfs
real(prec)::fac
real(prec)::lexp(nt),ucsr
integer::tc,ic,jc			! Counters

real(prec)::wght

! Read in (age-specific) Population Numbers / Familiy Size
if (optconstmort) then
	open(50,file='uspop1.txt')
	read(50,*) usp
	close(50)

	open(50,file='uschildren1.txt')
	read(50,*) tempfs
	close(50)

else
	open(50,file='uspop0.txt')
	read(50,*) usp
	close(50)
	
	open(50,file='uschildren0.txt')
	read(50,*) tempfs
	close(50)

endif


do tc=1,nt
	! population
	do jc=1,nj
		pop(tc,jc)=hwght*usp(age0+jc-1,tc)+(1.0-hwght)*usp(age0+jc-1,1)
		fsize(tc,jc)=hwght*tempfs(age0+jc-1,tc)+(1.0-hwght)*tempfs(age0+jc-1,1)
	end do
	topop(tc)=sum(pop(tc,:))
	wapop(tc)=sum(pop(tc,1:jr-1))
	retpop(tc)=topop(tc)-wapop(tc)
end do
wapr(:)=wapop(:)/topop(:)
oadr(:)=retpop(:)/wapop(:)

! effective family size
fsize(:,:) = 1.0+kappa*fsize(:,:)


! Survival probabilities: surv(j) = prob(alive in j | alive in j-1)
do tc=2,nt
	do jc=2,nj
		surv(tc,jc) = min(pop(tc,jc)/pop(tc-1,jc-1),1.0)
	end do
	surv(tc,1) = 1.0
end do
surv(1,:)=surv(2,:)

! life expectancy
do tc=1,nt
	ucsr=1.0
	lexp(tc)=0.0
	do jc=2,nj
		ucsr=ucsr*surv(tc,jc)
		lexp(tc)=lexp(tc)+ucsr
	end do
	lexp(tc)=lexp(tc)+age0
end do


! Construct work-retirement index
eta(1:jr-1)=1
eta(jr:nj)=0

! Labor in exogenous employment model
!if (not(optendlab)) then 
	do tc=1,nt
		do ic=1,ni
			lab_k(tc,ic,1:jr-1)=lab0
			lab_k(tc,ic,jr:nj)=0.0
		end do
	end do
!endif


end subroutine demographics