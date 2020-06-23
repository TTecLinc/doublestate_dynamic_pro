subroutine guess

use params
implicit none

integer::ic,kc,jc,tc,it
integer::bk,k1,k2,j0
real(prec)::equilinpt(7,nt)
!real(prec)::lcinpt(13,nt*ni*nj)
real(prec)::lcinpt(12,nt*ni*nj)
real(prec)::socsecinpt(2,nt)

if ((optstval==1) .and. not(optonlystst)) then
	capint(:)=Fcapint(ky0)	
	bqr(:)=0.05774746
	cJr(:)=0.08129609
	if (opteduc) then
		hJr(:)=1.19439113
		hbar(:)=1.55520188
	! else: see housegiven
	endif
else
	! input of equilibrium values from earlier iteration
	if (optonlystst) then
		open(99,file='ststequil.txt')
		read(99,*) equilinpt
		close(99)
		! for steady state comparison
		if (opteduc) then
			if (optfixrr .and. optexogeduc) then
				open(98,file='fixrrlc.txt')	! load educ-data from GE-model
				read(98,*) lcinpt
				close(98)
			elseif (not(optfixrr)   .and. optexogeduc) then
				open(98,file='lc.txt')		! load educ-data from GE-model
				read(98,*) lcinpt
				close(98)
			endif
		endif
	elseif (opteduc) then
		if (optfixrr) then
			if (optexogeduc) then
				open(99,file='exogeducfixrrequil.txt')
				! open(99,file='fixrrequil.txt')
				read(99,*) equilinpt
				close(99)

				open(98,file='fixrrlc.txt')
				read(98,*) lcinpt
				close(98)

				open(99,file='exogeducsocsec.txt')
				read(99,*) socsecinpt
				close(99)
			else
				open(99,file='fixrrequil.txt')
				read(99,*) equilinpt
				close(99)

				open(99,file='socsec.txt')
				read(99,*) socsecinpt
				close(99)

			endif
		else
			if (optexogeduc) then
				! open(99,file='equil.txt')
				open(99,file='exogeducequil.txt')
				read(99,*) equilinpt
				close(99)

				open(98,file='lc.txt')
				read(98,*) lcinpt
				close(98)
			else
				open(99,file='equil.txt')
				read(99,*) equilinpt
				close(99)
			endif
		endif
	else	! if not(opteduc) 
		if (optfixrr) then
			! open(99,file='noeducequil.txt')
			open(99,file='noeducfixrrequil.txt')
			read(99,*) equilinpt
			close(99)

			open(98,file='fixrrlc.txt')
			read(98,*) lcinpt
			close(98)
		else
			! open(99,file='equil.txt')
			open(99,file='noeducequil.txt')
			read(99,*) equilinpt
			close(99)

			open(98,file='lc.txt')
			read(98,*) lcinpt
			close(98)
		endif
	endif

	capint(:)=equilinpt(1,:)
	bqr(:)=equilinpt(2,:)
	cJr(:)=equilinpt(5,:)
	if (opteduc) then
		hJr(:)=equilinpt(6,:)
		hbar(:)=equilinpt(7,:)
	! else: see housegiven
	endif

	! pension system
	if ( optpens ) then

		if (optfixrr) then
			taups(:)=equilinpt(3,:)
			rrps(:)=socsecinpt(2,:)
		endif

		if ( optfixrr ) then
			taups(:)=equilinpt(3,:)
			rrps(btss+1:nt)=rrps(btss)
		else
			rrps(:)=equilinpt(4,:)
			taups(btss+1:nt)=taups(btss)
		endif
		!where (rrps(:)==0) rrps(:)=5.0e-02
	else
		rrps(:)=0.0
		taups(:)=0.0
	endif

	if ( not(opteduc) ) then

		it=1
		do kc=1,nt
			do ic=1,ni
				do jc=1,nj
					hk_k(kc,ic,jc)=lcinpt(3,it)
					it=it+1
				end do
			end do
		end do
	
		! hold hk constant at average decisions:
		do ic=1,ni
			do jc=1,nj
				! average hk of cohorts born during calibration period 
				hk_k(:,ic,jc)=sum(hk_k(tcal0:tcal1,ic,jc))/(tcal1-tcal0+1) 
			end do
		end do
	endif

	if (optexogeduc) then

		it=1
		do ic=1,ni
			do kc=1,nt
				do jc=1,nj
					hk_k(kc,ic,jc)=lcinpt(3,it)
					edu_k(kc,ic,jc)=lcinpt(4,it)
					it=it+1
				end do
			end do
		end do

	
		! hold educ constant at average decisions:
		do ic=1,ni
			do jc=1,nj
				! average educ of cohorts born during calibration period 
				hk_k(:,ic,jc)=sum(hk_k(tcal0:tcal1,ic,jc))/(tcal1-tcal0+1)
				edu_k(:,ic,jc)=sum(edu_k(tcal0:tcal1,ic,jc))/(tcal1-tcal0+1)
			end do
		end do
		
	endif

endif

! scaling factors
if (optsf==1) then
	sfk=min(1.0/capint(1),1.0)
	sfb=min(1.0/bqr(1),1.0)
	sfp=min(1.0/taups(1),1.0)
	sfhb=min(1.0/hbar(1),1.0)
else 
	sfk=1.0/capint(1)
	sfb=1.0/bqr(1)
	sfp=1.0/taups(1)
	sfhb=1.0/hbar(1)
endif

end subroutine guess