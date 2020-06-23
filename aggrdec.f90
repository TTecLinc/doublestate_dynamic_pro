subroutine aggrdec(t1,t2)

use params
implicit none

integer::tcm1,t1,t2
integer::ic,jc,tc,kc		! Counters
real(prec)::diff
real(prec),parameter::epsi=1.0e-02


! Compute aggregate variables
do tc=t1,t2
	if (t1==t2) then
		tcm1=tc
	elseif (tc==1) then
		tcm1=1
	elseif (tc==nt) then
		tcm1=tc-1
	else
		tcm1=tc-1
	endif
		
	! aggregation
	aggass(tc)=0.0
	agghrs(tc)=0.0
	aggeffhrs(tc)=0.0
	agghk(tc)=0.0
	aggcons(tc)=0.0
	aggedu(tc)=0.0
	aggtrans(tc)=0.0
	aggedu(tc)=0.0
	agggcons(tc)=0.0
	pens(tc)=0.0
	epens(tc)=0.0
	fpens(tc)=0.0
	agglstranf(tc) = 0.0
	do jc=1,nj	! age
		
		! cohort index
		if (t1==t2) then
			kc=tc
		else
			kc=max(cohind(tc,jc),1)
		endif

		do ic=1,ni	! type
			if (jc>1) then
				aggass(tc)=aggass(tc)+pop(tcm1,jc-1)*ass_k(kc,ic,jc)*At(tc)
				aggtrans(tc)=aggtrans(tc)+pop(tcm1,jc-1)*(1.0-surv(tc,jc))*ass_k(kc,ic,jc)*At(tc)
			endif
			aggcons(tc)=aggcons(tc)+pop(tc,jc)*cons_k(kc,ic,jc)*At(tc)
			agghrs(tc)=agghrs(tc)+pop(tc,jc)*lab_k(kc,ic,jc)				
			agghk(tc)=agghk(tc)+pop(tc,jc)*hk_k(kc,ic,jc)				
			aggeffhrs(tc)=aggeffhrs(tc)+pop(tc,jc)*lab_k(kc,ic,jc)*hk_k(kc,ic,jc)				
			aggedu(tc)=aggedu(tc)+pop(tc,jc)*edu_k(kc,ic,jc)				
			epens(tc)=epens(tc)+pop(tc,jc)*epens_k(kc,ic,jc)
			fpens(tc)=fpens(tc)+pop(tc,jc)*fpens_k(kc,ic,jc)
			agglstranf(tc) = agglstranf(tc) + pop(tc,jc)*lstr_k(kc,ic,jc)*At(tc) 
		end do
	
	end do		! end do jc
	pens(tc)=epens(tc)+fpens(tc)	
	bqr(tc)=(aggtrans(tc)/topop(tc)) / (At(tc)*h0*netwage(tc))		! per capita bequests 

	aggcap(tc)=aggass(tc)
	if ( (optsmallopen) .and. (tc>bt) ) then
		capint(tc)=capint(bt)
		aggcap(tc)=capint(tc)*(At(tc)*aggeffhrs(tc))
	else
		capint(tc)=aggcap(tc)/(At(tc)*aggeffhrs(tc))
	endif
	aggprod(tc)=F(At(tc),aggcap(tc),aggeffhrs(tc))
	
	! investment
    if (t1==t2) then		! steady state computation
	    agginv(tc)=(gt(tc)+deltakt(tc))*aggcap(tc)
	else
        agginv(tcm1)=aggcap(tc)-aggcap(tcm1)*(1.0-deltakt(tcm1))
    endif
	
	! hbar(tc)=aggeffhrs(tc)/(agghrs(tc)*h0)		! normalize by h0 (old definition, just another way to write hbar)
	hbar(tc)=aggeffhrs(tc)/wapop(tc)				! definition from paper
		
	agggcons(tc)=agggcons(tc)+aggass(tc)*ret(tc)*ctax
	if (optbq==1) then
		agggcons(tc)=agggcons(tc)+aggtrans(tc)
	endif

	! update ratios for disaggregate variables
	cJr(tc)=cJ(tc)/(netwage(tc)*h0)		! noramlize also by h0
	hJr(tc)=hJ(tc)/(netwage(tc)*h0)

end	do	! end do tc
agginv(nt)=agginv(nt-1)*aggprod(nt)/aggprod(nt-1)	! steady state investment increases with trend growth rate

end subroutine aggrdec



