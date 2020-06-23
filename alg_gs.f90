SUBROUTINE alg_gs(fname,fvec,x,n,check,gswght,MAXITS,TOLF)

use params, only: prec

IMPLICIT NONE

INTEGER :: i,its,n,MAXITS
REAL(prec) :: TOLF,f,fold,stpmax,gswght
REAL(prec), DIMENSION(n) :: x,fvec,xold,p,xn
REAL(prec), PARAMETER :: EPS=epsilon(x),TOLX=1.0e-4
LOGICAL :: check
external fname

call fname(fvec,x,n)

! Check convergence
print*,'Error in iteration # 1: ',maxval(abs(fvec(:)))


! Test for initial guess being a root.
! Use more stringent test than simply TOLF.
if (maxval(abs(fvec(:))) < TOLF) then			
	check=.false.
	RETURN
endif

do its=1,MAXITS										! Start of iteration loop
	xold(:)=x(:)													! Store x, F, and f.
	
	p=-gswght*fvec
	x=xold+p
	call fname(fvec,x,n)
	print*,'Error in iteration # ',its+1, ':', maxval(abs(fvec(:)))

	if (maxval(abs(fvec(:))) < TOLF) then							! Test for convergence on function values.
		check=.false.
		RETURN
	elseif ( maxval(abs(x(:)-xold(:))) < TOLX) then					! Test for convergence on dx: Here: absolute deviation 
		check=.false.
		return				
	endif
	
end do
check=.true.
print*,'MAXITS exceeded in ALG_GS'

end subroutine alg_gs

