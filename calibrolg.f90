subroutine calibrolg

! calibration of OLG model

use params

implicit none

real(prec),allocatable::paraminpt(:,:)
real(prec),allocatable::mmfacinpt(:,:)
real(prec),allocatable::coeffs(:),fval(:),df(:,:)
real(prec),dimension(2,nt)::tddat
real(prec)::mdeltakt,mgt,dist
integer::ic,tc,tcc,maxfn,nm
logical::check,intlj,reevalj
external olg


! number of moments, nm, for calibration:
if (optcalibr==0) then
	nm=6
	if (optbetta==1) then
		nm=nm+1
	elseif (optbetta==2) then
		nm=nm+2
	endif
else
	nm=2		! betta, deltak
	if (optendlab) then
		nm=nm+1						! phii: consumption share parameter
	endif
	if (optcalibr==2) then
		nm=nm+3					! deltah,xii,psii
	endif
	if (optbetta==1) then
		nm=nm+1					! coefficients in betta vector
	elseif (optbetta==2) then
		nm=nm+2
	endif
endif
allocate(coeffs(nm))
allocate(fval(nm))
allocate(df(nm,nm))
allocate(sfcalibr(nm))
allocate(mmfac(nm))
allocate(mmfacinpt(nm,1))
allocate(paraminpt(2,nm))

! input of moment factors
if (optcalibr>0 .and. optonlystst) then
	! moment factors
	if (optbetta==0) then
		open(50,file='calibrmmfac.txt')
	elseif (optbetta==1) then
		open(50,file='calibrmmfacbeta1.txt')
	else 
		open(50,file='calibrmmfacbeta2.txt')
	endif
	read(50,*) mmfacinpt
	close(50)

	do ic=1,nm
		mmfac(ic)=mmfacinpt(ic,1)
	end do
else
	mmfac(:)=1.0
endif

! make coefficient vector
if (optincalibr) then
	print*, 'input of calibration parameters from external file?'
	print*, 'press any key to continue'
	pause
	if (optbetta==0) then
		open(50,file='coeffs.txt')
	elseif (optbetta==1) then
		open(50,file='coeffsbeta1.txt')
	else	
		open(50,file='coeffsbeta2.txt')
	endif
	read(50,*) paraminpt
	close(50)
	coeffs(:)=paraminpt(1,:)

	! store coefficients
	! (default, in case optcalibr=0)
	betta=coeffs(1)
	deltak0=coeffs(2)
	phi=coeffs(3)
	xii=coeffs(4)
	psii=coeffs(5)
	deltah=coeffs(6)
	if (optbetta==1) then
		bettacoeffs(1)=coeffs(7)
		bettacoeffs(2)=0.0
	elseif (optbetta==2) then
		bettacoeffs(1)=coeffs(7)
		tauls=coeffs(8)
	endif

else
	coeffs(1)=betta
	coeffs(2)=deltak0
	if (nm>1) then
		coeffs(3)=phi
	endif
	if (nm>3) then
		coeffs(4)=xii
		coeffs(5)=psii
		coeffs(6)=deltah
	endif
	if (optbetta==1) then
		coeffs(7)=bettacoeffs(1)
	elseif (optbetta==2) then
		coeffs(7)=bettacoeffs(1)
		coeffs(8)=tauls
	endif
endif
sfcalibr=1.0/coeffs
coeffs=coeffs*sfcalibr

! input of time dependent rates
gt(:)=g0
deltakt(:)=deltak0
if (opttd>0) then
	open(51,file='tfpgrdepr.txt')
	read(51,*) tddat
	close(51)

	gt(:)=tddat(1,:)
	if (opttd==2) then
		deltakt(:)=tddat(2,:)
	endif
endif

if (optcalibr>0) then
	! solution of model
	intlj=.true.
	reevalj=.true.
	call broyden(olg,fval,coeffs,nm,df,intlj,reevalj,check,maxstp,tolcalibr)

	print*, ' '
	print*, '----------------------'
	print*, 'calibration terminated'
	if (check) then
		print*, 'FAILURE to converge in broyden'
	else
		coeffs=coeffs/sfcalibr
		if ( maxval(abs(fval)) < tolcalibr ) then
			print*, 'with SUCCESS'
		else
			print*, 'convergence on x'
		endif
		do ic=1,nm
			print*, 'final function value ', ic, 'is ', fval(ic)
			print*, 'final coefficient estimate ', ic, 'is: ', coeffs(ic)
		end do
	endif
	print*, '----------------------'
	print*, ' '
else
	call olg(fval,coeffs,nm)
endif

deallocate(coeffs)
deallocate(fval)
deallocate(df)
deallocate(mmfac)
deallocate(sfcalibr)
deallocate(mmfacinpt)
deallocate(paraminpt)

end subroutine calibrolg

