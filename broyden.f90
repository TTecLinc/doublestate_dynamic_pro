SUBROUTINE broyden(fname,fvec,x,n,df,intlj,reevalj,check,maxstp,TOLF)

! This is a modified Broyden algorithm 
! - initialj=.true.:	initialization of Jacobi by first difference, reupdates by first differences
! - reevalj=.true.:		reupdates by first differences but not initially if initialj=0
! - if reevalj .and. initialj are .false., then reupdates are done using the initial Jacobi matrix
!	which is passed to the function in df

! Given an initial guess x for a root in N dimensions, find the root by Broyden’s method
! embedded in a globally convergent strategy. The length N vector of functions to be zeroed,
! called fvec in the routine below, is returned by a user-supplied routine that must be
! called funcv and have the declaration FUNCTION funcv(x). The subroutine fdjac and
! the function fmin from newt are used. The output quantity check is false on a normal
! return and true if the routine has converged to a local minimum of the function fmin or if
! Broyden’s method can make no further progress. In this case try restarting from a different
! initial guess.
! Parameters: MAXITS is the maximum number of iterations; EPS is the machine precision;
! TOLF sets the convergence criterion on function values; TOLMIN sets the criterion for deciding
! whether spurious convergence to a minimum of fmin has occurred; TOLX is the
! convergence criterion on x; 

! use maxstp instead of STPMX for maximum step in line-searches

use params, only: prec
USE alexutils, ONLY: get_diag,lower_triangle, outerprod,put_diag,unit_matrix
use alexfuncs, ONLY: qrdcmp,qrupdt,rsolv

IMPLICIT NONE

INTEGER :: i,its,k,n
REAL(prec) :: x(n)
LOGICAL :: check
INTEGER, PARAMETER :: MAXITS=200
REAL(prec), PARAMETER :: EPS=epsilon(x),TOLMIN=1.0e-6,TOLX=1.0e-4
real(prec)::TOLF
REAL(prec) :: f,fold
REAL(prec), DIMENSION(n):: fvec
REAL(prec), DIMENSION(n) :: c,d,fvcold,g,p,s,t,w,xold
REAL(prec), DIMENSION(n,n) :: r,qt,df,dfold
real(prec)::vabs,maxstp
LOGICAL :: restrt,sing,intlj,reevalj
external fname

restrt=.true.
call fminbr(fname,f,fvec,x,n)

! Test for initial guess being a root.
! Use more stringent test than simply TOLF.
if (maxval(abs(fvec(:))) < TOLF) then			
	check=.false.
	RETURN
endif

dfold=df
do its=1,MAXITS										! Start of iteration loop.
	if (restrt) then
		if (intlj) then								! initialj=1 if initial Jacobi evaluation is required
			call fdjac(fname,x,fvec,df)
			print*, ' '
			print*, '-------------------------------'
			print*, 'done with Jacobi initialization'
			print*, '-------------------------------'
			print*, ' '
		elseif (reevalj) then
			intlj=.true.							! make sure to reupdate when next required
		else
			df=dfold								! make sure to always reinitialize with old Jacobi matrix
		endif
		r=df										! Initialize or reinitialize Jacobian in r.
		call qrdcmp(r,c,d,sing)										! QR decomposition of Jacobian.
		if (sing) then
			print*,'singular Jacobian in broyden'
			return
		endif
		call unit_matrix(qt)										! Form QT explicitly.
		do k=1,n-1
			if (c(k) /= 0.0) then
				qt(k:n,:)=qt(k:n,:)-outerprod(r(k:n,k),& 
					matmul(r(k:n,k),qt(k:n,:)))/c(k)
			endif
		end do
		where (lower_triangle(n,n)) r(:,:)=0.0
		call put_diag(d(:),r(:,:))									! Form R explicitly.
	else															! Carry out Broyden update.
		s(:)=x(:)-xold(:)		
		do i=1,n 
			t(i)=dot_product(r(i,i:n),s(i:n))
		end do
		w(:)=fvec(:)-fvcold(:)-matmul(t(:),qt(:,:)) 
		where (abs(w(:)) < EPS*(abs(fvec(:))+abs(fvcold(:)))) &
			w(:)=0.0												! Don’t update with noisy components of w
		if (any(w(:) /= 0.0)) then
			t(:)=matmul(qt(:,:),w(:)) 
			s(:)=s(:)/dot_product(s,s)							
			call qrupdt(r,qt,t,s)									! Update R and QT .
			d(:)=get_diag(r(:,:))									! Diagonal of R stored in d.
			if (any(d(:) == 0.0)) &
				print*, 'r singular in broyden'
		endif
	endif
	p(:)=-matmul(qt(:,:),fvec(:))									! r.h.s. for linear equations 
	do i=1,n														! Compute gradient for the line search
		g(i)=-dot_product(r(1:i,i),p(1:i))
	end do
	xold(:)=x(:)													! Store x, F, and f.
	fvcold(:)=fvec(:)
	fold=f
	call rsolv(r,d,p)												! Solve linear equations for step
	
	! lnsrch returns new x and f. It also calculates fvec at the new x when it calls fmin.
	call lnsrch(xold,fold,g,p,x,n,f,fvec,check,fname,maxstp)
	
	if (maxval(abs(fvec(:))) < TOLF) then							! Test for convergence on function values.
		check=.false.
		RETURN
	endif
	if (check) then													! True if line search failed to find a new x
		! If restrt is true we have failure: We have already tried reinitializing the Jacobian.
		! The other test is for gradient of f zero, i.e., spurious convergence.
		! restrt=.true. Try reinitializing the Jacobian.
		if (restrt .or. maxval(abs(g(:))*max(abs(x(:)), &
			1.0)/max(f,0.5*n)) < TOLMIN) then
			print*,'failure in broyden, try reinitilization' 
			return
		endif
	else															! Successful step; will use Broyden update
		restrt=.false.
		if ( maxval(abs(x(:)-xold(:))) < TOLX) then					! Test for convergence on dx: Here: absolute deviation 
			return				
		endif
	endif
end do
check=.true.
print*,'MAXITS exceeded in broyden'

contains

! ---------------------------------------------
SUBROUTINE lnsrch(xold,fold,g,p,x,n,f,fvec,check,fname,maxstp)
					
! Given an N-dimensional point xold, the value of the function and gradient there, fold
! and g, and a direction p, finds a new point x along the direction p from xold where the
! function fname has decreased ¡°sufficiently.¡± xold, g, p, and x are all arrays of length N.
! The new function value is returned in f. maxstp is an input quantity that limits the length
! of the steps so that you do not try to evaluate the function in regions where it is undefined
! or subject to overflow. p is usually the Newton direction. The output quantity check is
! false on a normal exit. It is true when x is too close to xold. In a minimization algorithm,
! this usually signals convergence and can be ignored. However, in a zero-finding algorithm
! the calling program should check whether the convergence is spurious.
! Parameters: ALF ensures sufficient decrease in function value; TOLX is the convergence
! criterion on .x.

use params, only: prec,tolcalibr
IMPLICIT NONE
integer:: n,ndum,i
REAL(prec), DIMENSION(n) :: xold,g,p,fvec,x
REAL(prec) :: fold,f,vabs
LOGICAL :: check
REAL(prec), PARAMETER :: ALF=1.0e-4,TOLX=epsilon(x)
REAL(prec) :: a,alam,alam2,alamin,b,disc,f2,pabs,rhs1,rhs2,slope,tmplam,maxstp
external fname

check=.false.

! check for maximum step
do i=1,n
	if (abs(p(i))>maxstp) then
		p=p*maxstp/abs(p(i))
	endif
end do

slope=dot_product(g,p)
if (slope >= 0.0) print*, 'roundoff problem in lnsrch'

alamin=TOLX/maxval(abs(p(:))/max(abs(xold(:)),1.0))			! Compute alammin
alam=1.0														! Always try full Newton step first.
do 
	x(:)=xold(:)+alam*p(:)
	call fminbr(fname,f,fvec,x,n)
	if (alam < alamin) then										! Convergence on x
		x(:)=xold(:)
		check=.true.
		RETURN
	elseif (f <= fold+ALF*alam*slope) then						! Sufficient function decrease.
		RETURN
	else														! Backtrack.
		if (alam == 1.0) then									! First time.
			tmplam=-slope/(2.0*(f-fold-slope))
		else													! Subsequent backtracks.
			rhs1=f-fold-alam*slope
			rhs2=f2-fold-alam2*slope
			a=(rhs1/alam**2-rhs2/alam2**2)/(alam-alam2)
			b=(-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/(alam-alam2)
			if (a == 0.0) then
				tmplam=-slope/(2.0*b)
			else
				disc=b*b-3.0*a*slope
				if (disc < 0.0) then
					tmplam=0.5*alam
				elseif (b <= 0.0) then
					tmplam=(-b+sqrt(disc))/(3.0*a)
				else
					tmplam=-slope/(b+sqrt(disc))
				end if
			end if
			if (tmplam > 0.5*alam) tmplam=0.5*alam 
		end if
	end if
	alam2=alam
	f2=f
	alam=max(tmplam,0.1*alam) 
end do 

END SUBROUTINE lnsrch
! ---------------------------------------------


! ---------------------------------------------
subroutine fminbr(fname,f,fvec,x,n)

use params, only: prec
implicit none

integer::n
real(prec)::x(n),fvec(n),f
external fname

call fname(fvec,x,n)
f=0.5*dot_product(fvec,fvec)

end subroutine fminbr
! ---------------------------------------------


! ---------------------------------------------
SUBROUTINE fdjac(fname,x,fvec,df)

! Computes forward-difference approximation to Jacobian. On input, x is the point at which
! the Jacobian is to be evaluated, and fvec is the vector of function values at the point,
! both arrays of length N. df is the N × N output Jacobian. FUNCTION funcv(x) is a
! fixed-name, user-supplied routine that returns the vector of functions at x.
! Parameter: EPS is the approximate square root of the machine precision.

IMPLICIT NONE
REAL(prec), DIMENSION(:), INTENT(IN) :: fvec
REAL(prec), DIMENSION(:), INTENT(INOUT) :: x
REAL(prec), DIMENSION(:,:), INTENT(OUT) :: df
REAL(prec), DIMENSION(size(x)) :: funcv
REAL(prec), PARAMETER :: EPS=1.0e-3
INTEGER :: j,n
REAL(prec), DIMENSION(size(x)) :: xsav,xph,h
EXTERNAL fname

n=size(x)
xsav=x
h=EPS*abs(xsav)
where (h == 0.0) h=EPS
xph=xsav+h								! Trick to reduce finite precision error.
h=xph-xsav
do j=1,n
	x(j)=xph(j)
	call fname(funcv,x,n)
	df(:,j)=(funcv-fvec(:))/h(j)			! Forward difference formula.
	x(j)=xsav(j)
end do

END SUBROUTINE fdjac
! ---------------------------------------------


end subroutine broyden