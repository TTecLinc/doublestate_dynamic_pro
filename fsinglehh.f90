! ------------------------------------------------------
function fsinglehh(xhh)

! solution of single households problem

use params
use paramshh
implicit none

real(prec)::xhh,fsinglehh
real(prec)::bettatilde,marguc,marguleis,margucp1,pincr,help,help1,help2
real(prec)::Vap1,Vhp1,Vsp1
real(prec),dimension(nj)::lcr
real(prec)::dgdh,dgde
real(prec)::PVcons,PVinc,PVpens,PVwage,PVtrans,PVlstrans
real(prec)::consgr(nj-1),dfac,gfac,piafac(nj),adjfac
real(prec)::foc(3)
integer::sa,sb
integer::jc,jrr,jcu,jcl
real(prec),parameter::epsi=1.0e-06
real(prec)::bettaage

! settings for solver dzreal:
real(prec),parameter::pdhk=0.01
real(prec)::hkmax,fhk,dhk
real(prec),parameter::errabs=1.0e-6
real(prec),parameter::errrel=1.0e-6
integer,parameter::maxfn=20
integer,parameter::nroot=1
real(prec),parameter::etta=0.01
real(prec),parameter::eps=1.0e-8
integer::info(nroot)


! default of education and human capital for all j 
! (relevant for case of only endogenous labor)
if (not(opteduc)) then
	edu_j(:)=0.0
endif
cJ0=xhh

! initialization
cons_j(nj)=cJ0
if (optendlab) then
    if ( opteduc .and. not(optexogeduc) ) then
		hk_j(nj)=hJ0
		edu_j(nj)=0.0
	endif
	if (jr>nj) then		! if there is no retirement period
		help=0.0
		lcr(nj)=Flcr(hk_j(nj),netw_j(nj),help)
		leis_j(nj)=lcr(nj)*cons_j(nj)
		if (leis_j(nj)>1.0) then
			leis_j(nj)=1.0
		endif
		lab_j(nj)=1.0-leis_j(nj)
	else
		leis_j(nj)=1.0
		lab_j(nj)=0.0
	endif
else   ! if not(ope_end_lab)
    leis_j(nj)=1.0-lab_j(nj)
endif
lcr(nj)=leis_j(nj)/cons_j(nj)
val_j(nj)=util(cons_j(nj),leis_j(nj),gamtild_j(nj))

! adjustment factor to rate of return
if (optbq==3) then		! adjust rate of return by survival rates in case of annuity markets
	Rgross =  (1.0+netr_j(:))/sr_j(:) 
else
	Rgross = 1.0+netr_j(:) 
endif 

! compute primary insurance amount factor
gfac=1.0
if (j0>jr) then
	gfac=(1.0+g_j(j0))**(j0-jr)
endif
jrr=max(j0,jr)
piafac(jrr:nj)=wage_jr*hbar_jr*1.0/(jr-1)
if ( optrr==1 ) then	! cohort specific replacment rate
	piafac(jrr:nj)=rrps_jr*piafac(jrr:nj)
else					! time specific replacement rate
	piafac(jrr:nj)=rrps_j(jrr:nj)*piafac(jrr:nj)
endif
do jc=jrr,nj
	piafac(jc)=1.0/gfac*piafac(jc)
	if (jc<nj) then
		gfac=gfac*(1.0+g_j(jc+1))
	endif
end do


call margu(cons_j(nj),leis_j(nj),gamtild_j(nj),marguc,marguleis)
Vap1 = Rgross(nj)*marguc
Vhp1 = 0.0
Vsp1 = 0.0
do jc = nj-1,j0,-1
    ! adjusted discount factor
	bettaage=fbettaage(jc)
	bettatilde = bettaage*(1.0+g_j(jc+1))**(phi*(1.0-tetta))*sr_j(jc+1)
		
    ! solve for consumption
	marguc = bettatilde * 1.0/(1.0+g_j(jc+1)) * Vap1
    if (optendlab) then
		if ( opteduc .and. not(optexogeduc) ) then

			if (jc>jh) then
				edu_j(jc) = 0.0
				call hkfunc(edu_j(jc),hk_j(jc),hk_j(jc+1),2)
			else
				! upper bound for human capital at edu=0.0
				edu_j(jc)=0.0
				call hkfunc(edu_j(jc),hkmax,hk_j(jc+1),2)		

				! search in neighbourhood:
				dhk=pdhk*hkmax
				call dzreal(funchk,errabs,errrel,eps,etta,nroot,maxfn,dhk,dhk,info)
				! fhk=funchk(hksc)
				hk_j(jc)=hkmax-dhk

				! compute asscoiated education decision
				call hkfunc(edu_j(jc),hk_j(jc),hk_j(jc+1),0)

			endif
		
		endif	! end if opteduc
		
		! compute consumption and leisure
		if (jc>=jr) then
			leis_j(jc)=1.0
			cons_j(jc) = Fcons(marguc,leis_j(jc),gamtild_j(jc),0)
		else
			help=(1+g_j(jc+1))*Vsp1/Vap1*1.0/hbar_j(jc)
			lcr(jc)=Flcr(hk_j(jc),netw_j(jc),help)
			cons_j(jc) = Fcons(marguc,lcr(jc),gamtild_j(jc),1)
			leis_j(jc)=lcr(jc)*cons_j(jc)
			lab_j(jc)=1.0-leis_j(jc)-edu_j(jc)
			if (lab_j(jc)<0.0) then
				lab_j(jc)=0.0
				leis_j(jc)=1.0-edu_j(jc)	
				cons_j(jc) = Fcons(marguc,leis_j(jc),gamtild_j(jc),0)
			endif
		endif
		
	else   ! if not(opt_end_lab)
        leis_j(jc) = 1.0-lab_j(jc)
        cons_j(jc) = Fcons(marguc,leis_j(jc),gamtild_j(jc),0)
    endif
	lcr(jc)=leis_j(jc)/cons_j(jc)

    ! update marginal utility and derivatives of education function:
	call margu(cons_j(jc),leis_j(jc),gamtild_j(jc),marguc,marguleis)
	call derivg(hk_j(jc),edu_j(jc),dgde,dgdh)    
	
	! test focs
	call hhfoc(foc,marguc,marguleis,dgde,1)
	
	! test of time constraint
	if ( abs(1.0-leis_j(jc)-edu_j(jc)-lab_j(jc)) > 0.0001 )  then
		print*, 'leisure and education do not add to one for age ', jc
		! pause
	endif

    ! update derivatives of value function:
    help=1.0/(1.0+g_j(jc+1))*Vap1
	Vhp1 = bettatilde * ( lab_j(jc)*(netw_j(jc)*help+1.0/hbar_j(jc)*Vsp1) + dgdh*Vhp1 )
	Vsp1 = bettatilde*Vsp1
	if (jc>=jr) then
		Vsp1 = Vsp1 + bettatilde*lamb*piafac(jc)*help
	endif
	Vap1 = Rgross(jc)*marguc

	! update life-time utility
	val_j(jc) = util(cons_j(jc),leis_j(jc),gamtild_j(jc)) + bettatilde*val_j(jc+1)	
end do   ! end for jc

! compute lump-sum transfers
if ( (j0==1) .and. (tauls>0.0) ) then
	! lumpsum transfers to workers:
	lstr_j(1:jr-1)=tauls*w_j(1:jr-1)


	! discount factor 
	dfac=1.0
	PVlstrans=0.0
	do jc=1,(jr-1)
		PVlstrans = PVlstrans + lstr_j(jc)/dfac
		dfac=dfac*Rgross(jc+1)/(1.0+g_j(jc+1))
	end do

	! compute transfers to pensioners
	adjfac=0.0
	do jc=jr,nj
		adjfac=adjfac+1.0/dfac
		if (jc<nj) then
			dfac=dfac*Rgross(jc+1)/(1.0+g_j(jc+1))
		endif
	end do
	lstr_j(jr:nj)=-PVlstrans/adjfac

! elseif (j0>1) then: lump-sum transfers are input

endif


! discount factor 
dfac=1.0

! compute assets and human capital:
ass_j(j0)=intlass
hk_j(j0)=intlhk
pstock_j(j0)=intlpstock
PVcons = 0.0
PVinc = 0.0
PVpens = 0.0
PVwage = 0.0
PVtrans = 0.0
PVlstrans = 0.0
do jc=j0,nj
	pincr=lab_j(jc)*hk_j(jc)/hbar_j(jc)
	if (jc==1) then
		pstock_j(jc)=pincr
	else
		pstock_j(jc)=pstock_j(jc-1)+pincr
	endif
	if (jc>=jr) then
		epens_j(jc)=lamb*piafac(jc)*pstock_j(jc)
		pens_j(jc)=epens_j(jc)+fpens_j(jc)
	else
		epens_j(jc)=0.0
		fpens_j(jc)=0.0
		pens_j(jc)=0.0
	endif

	! income and cash-on-hand
	inc_j(jc) = hk_j(jc)*netw_j(jc)*lab_j(jc)+pens_j(jc)+lstr_j(jc) 
	coh_j(jc) = ( ass_j(jc)+trans_j(jc) ) * Rgross(jc) + inc_j(jc)

	! present discount values
	PVcons = PVcons + cons_j(jc)/dfac
    PVpens = PVpens + pens_j(jc)/dfac
    PVwage = PVwage + hk_j(jc)*netw_j(jc)*lab_j(jc)/dfac
	PVtrans = PVtrans + trans_j(jc)*(1.0+netr_j(jc))/dfac
	PVlstrans = PVlstrans + lstr_j(jc)/dfac

	! assets and discount factor
	if (jc<nj) then
		ass_j(jc+1) = 1.0/(1+g_j(jc+1))* ( coh_j(jc) - cons_j(jc) )
		if (opteduc) then
			call hkfunc(edu_j(jc),hk_j(jc),hk_j(jc+1),1)
		endif
		dfac=dfac*Rgross(jc+1)/(1.0+g_j(jc+1))
	endif
end do

! check PVlstrans
if (abs(PVlstrans)>epsi) then
	print*, 'error: PVlstrans should be zero'
	pause
endif


! update consumption
PVinc = PVpens + PVwage + PVtrans + ass_j(j0)*(1+netr_j(j0))
if (j0<nj) then
	consgr(j0:nj-1) = cons_j(j0+1:nj)/cons_j(j0)
	cons_j(j0) = max(1.0E-08,cons_j(j0)*PVinc/PVcons)
	cons_j(j0+1:nj)=cons_j(j0)*consgr(j0:nj-1)
else
	cons_j(j0)=max(1.0E-08,PVinc)
endif

! function value:
fsinglehh = ( coh_j(nj) - cons_j(nj) ) / cJ0


contains



! --------------------------------------------------------------------------------------
function funchk(dhk0)

! use params
implicit none

real(prec)::funchk
real(prec),intent(in)::dhk0
real(prec)::help,help1,help2,dgde,dgdh,mu
real(prec)::epsi=1.0e-06

hk_j(jc)=hkmax-dhk0

! compute education
call hkfunc(edu_j(jc),hk_j(jc),hk_j(jc+1),0)
if (edu_j(jc)<0.0) then
	edu_j(jc)=0.0
elseif (edu_j(jc)>=1.0) then
	!edu_j(jc)=edu_j(jc)-epsi
	edu_j(jc) = 1.0 - epsi 
endif

! compute lcr and consumption
if (hk_j(jc)<0.0) then
	print*, 'error in code: HK should always be positive'
	pause
endif
help=(1+g_j(jc+1))*Vsp1/Vap1*1.0/hbar_j(jc)
lcr(jc)=Flcr(hk_j(jc),netw_j(jc),help)
cons_j(jc) = Fcons(marguc,lcr(jc),gamtild_j(jc),1)

! compute leisure and labor			
leis_j(jc)=lcr(jc)*cons_j(jc)
lab_j(jc) = 1.0 - leis_j(jc) - edu_j(jc)
if (lab_j(jc)<0.0) then
	lab_j(jc)=0.0
	leis_j(jc)=1.0-edu_j(jc)
	cons_j(jc) = Fcons(marguc,leis_j(jc),gamtild_j(jc),0)
	lcr(jc) = leis_j(jc)/cons_j(jc)	
	! Alternative concepts of distance function:
	! If you uncomment **** below than comment **
	! **
	call margu(cons_j(jc),leis_j(jc),gamtild_j(jc),marguc,marguleis)
	call derivg(hk_j(jc),edu_j(jc),dgde,dgdh)    
	call hhfoc(foc,marguc,marguleis,dgde,0)
	mu = -foc(2)
	! **
else
	mu=0.0
endif


! distance function	
! ****
! call margu(cons_j(jc),leis_j(jc),gamtild_j(jc),marguc,marguleis)
! call derivg(hk_j(jc),edu_j(jc),dgde,dgdh)    
! call hhfoc(foc,marguc,marguleis,dgde,0)
! funchk=foc(3)
! ****
help1 = bettatilde*xii*psii*hk_j(jc)**psii*Vhp1
help2 = bettatilde*hk_j(jc)* ( netw_j(jc)*Vap1/(1.0+g_j(jc+1)) + Vsp1*1.0/hbar_j(jc) ) + mu 		
funchk = edu_j(jc) - ( help1/help2 )**(1.0/(1.0-psii))			


end function funchk
! --------------------------------------------------------------------------------------


! -------------------------------------------------------------------------
subroutine margu(c,leis,gam,marguc,marguleis)

! calculate marginal utilities of consumption and leisure

implicit none

real(prec),intent(in)::c,leis,gam
real(prec),intent(inout)::marguc,marguleis
real(prec)::help

help = ( c**phi * leis**(1.0-phi) )**(1.0-tetta)

! marginal utility w.r.t. consumption and leisure:
marguc = gam*help*phi*c**(-1.0)
marguleis = gam*help*(1.0-phi)*leis**(-1.0)

end subroutine margu
! -------------------------------------------------------------------------



! --------------------------------------------------------------------------------------
subroutine derivg(h,e,dgde,dgdh)

! calculate derivative of education function with respect to h

implicit none

real(prec)::h,e,dgdh,dgde,help

if (e>=0.0) then	! careful w.r.t. subcases
	dgde = xii*psii*e**(psii-1.0)*h**psii
endif
dgdh = (1.0-deltah) + xii*psii*h**(psii-1.0)*e**psii

end subroutine derivg
! --------------------------------------------------------------------------------------


! --------------------------------------------------------------------------------------
function Flcr(hk0,w0,help)

! calculate leisure to consumption ratio

implicit none

real(prec)::Flcr
real(prec),intent(in)::w0,hk0,help
real(prec)::temp

temp=hk0*(w0+help)
Flcr = 1.0/temp * (1.0-phi)/phi 

end function Flcr
! --------------------------------------------------------------------------------------


! -------------------------------------------------------------------------
function Fcons(margu,x,gam,ind)

! calculate consumption from marginal utility

implicit none

real(prec)::Fcons
integer,intent(in)::ind
real(prec),intent(in)::margu,x,gam
real(prec)::help

help = margu/(phi*gam*x**((1.0-phi)*(1.0-tetta)))
if (ind==1) then		! endogenous labor supply, x=lcr
	Fcons = help**(-1.0/tetta)
else					! exogenous labor supply, x=leis
	Fcons = help**(1.0/(phi*(1.0-tetta)-1.0))
endif

end function Fcons
! -------------------------------------------------------------------------


! -------------------------------------------------------------------------
subroutine hhfoc(foc,marguc,marguleis,dgde,ind)

implicit none
real(prec),intent(inout)::foc(3)
real(prec),intent(in)::marguc,marguleis,dgde
integer,intent(in)::ind
real(prec)::help
real(prec),parameter::epsi=0.01
real(prec),parameter::tol=1.0e-04

foc(:)=0.0
help=1.0/(1.0+g_j(jc+1))*Vap1
foc(1) = marguc - bettatilde*help
if (optendlab) then
	foc(2) = -marguleis + bettatilde*hk_j(jc)* ( netw_j(jc)*help + Vsp1*1.0/hbar_j(jc) )
endif
if ( opteduc .and. not(optexogeduc) ) then
    foc(3) = -marguleis + bettatilde*dgde*Vhp1
endif


if (ind==0) return

! Tests
if ( abs(foc(1))>tol ) then
	print*, 'foc(1) fails to hold for age ', jc
	! pause
endif
if ( optendlab .and. (lab_j(jc)>0.0) .and. abs(foc(2))>tol ) then
	print*, 'foc(2) fails to hold for age ', jc
	! pause
endif
if ( opteduc .and. not(optexogeduc) .and. (edu_j(jc)>epsi) .and. abs(foc(3))>tol ) then
	print*, 'foc(3) fails to hold for age ', jc
	! pause
endif

end subroutine hhfoc
! -------------------------------------------------------------------------




end function fsinglehh
