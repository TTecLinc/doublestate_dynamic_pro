module alexutils

! these are a number of functions copied from the numerical recipes book

use params, ONLY: prec
implicit none

contains


! ---------------------------------------------
function get_diag(mat)
! input: matrix of arbitrary size
! output: diagonal elements of matrix

real(prec),dimension(:,:),INTENT(IN) :: mat
real(prec), dimension(min(size(mat,1),size(mat,2))) :: get_diag
integer j

do j=1,min(size(mat,1),size(mat,2))
	get_diag(j)=mat(j,j)
end do

end function
! ---------------------------------------------


! ---------------------------------------------
FUNCTION lower_triangle(j,k ,extra)
! (returns a lower triangular mask)

INTEGER, INTENT(IN) :: j,k
INTEGER, OPTIONAL, INTENT(IN) :: extra
LOGICAL, DIMENSION(j,k) :: lower_triangle

INTEGER :: n,jj,kk
n=0
if (present(extra)) n=extra
do jj=1,j
	do kk=1,k
		lower_triangle(jj,kk)= (kk-jj < n)
	end do
end do

END FUNCTION lower_triangle
! ---------------------------------------------


! ---------------------------------------------
FUNCTION outerprod(a,b)
! takes outer product of two vectors a and b
real(prec), DIMENSION(:), INTENT(IN) :: a,b
real(prec), DIMENSION(size(a),size(b)) :: outerprod

outerprod = spread(a,dim=2,ncopies=size(b)) * &
	spread(b,dim=1,ncopies=size(a))

END FUNCTION outerprod
! ---------------------------------------------


! ---------------------------------------------
FUNCTION ifirstloc(mask)

! Returns the index (subscript value) of the first location, in a one-dimensional
! logical mask, that has the value .TRUE., or returns size(mask)+1 if all
! components of mask are .FALSE.
! Note that while the reference implementation uses a do-loop, the function is
! parallelized in nrutil by instead using the merge and maxloc intrinsics.
! Reference implementation:

logical, dimension(:), INTENT(IN) :: mask
INTEGER:: ifirstloc
INTEGER:: i

do i=1,size(mask)
	if (mask(i)) then
		ifirstloc=i
		return
	end if
end do
ifirstloc=i

END FUNCTION ifirstloc
! ---------------------------------------------


! ---------------------------------------------
SUBROUTINE put_diag(diag,mat)

! Sets the diagonal of matrix mat equal to the argument diag, either a scalar
! or else a vector whose size must be the smaller of the two dimensions of
! matrix mat. The following shows an implementation where diag is a vector;
! the scalar case can be overloaded (see Appendix C1).
! Reference implementation:

real(prec), DIMENSION(:), INTENT(IN) :: diag
real(prec), DIMENSION(:,:), INTENT(INOUT) :: mat

INTEGER:: j,n,m

n=size(diag)
m=min(size(mat,1),size(mat,2))

if (n==m) then
	do j=1,n
		mat(j,j)=diag(j)
	end do
else	
	print*, 'error in put_diag: n not equal to m'
	return
endif

END SUBROUTINE put_diag
! ---------------------------------------------



! ---------------------------------------------
SUBROUTINE unit_matrix(mat)
real(prec), DIMENSION(:,:), INTENT(OUT) :: mat

! Sets the diagonal components of mat to unity, all other components to zero.
! When mat is square, this will be the unit matrix; otherwise, a unit matrix
! with appended rows or columns of zeros.

INTEGER:: i,n
n=min(size(mat,1),size(mat,2))

mat(:,:)=0.0
do i=1,n
	mat(i,i)=1.0
end do

END SUBROUTINE unit_matrix
! ---------------------------------------------


! ---------------------------------------------
function kronprod(A,B)

! Takes the Kronecker product of the two input matrices mat1 and mat2

real(prec), DIMENSION(:,:), INTENT(IN) :: A,B
real(prec), DIMENSION(size(A,1)*size(B,1),size(A,2)*size(B,2)) :: kronprod
INTEGER:: n,m,p,q,mp,nq,i,j,k,l

m=size(A,1)
n=size(A,2)
p=size(B,1)
q=size(B,2)


kronprod(:,:)=0.0
do i=1,m						! go down rows of A
	k=(i-1)*p+1					! fill in rows of B
	do j=1,n					! go down columns of A
		l=(j-1)*q+1				! fill in columns of B
		kronprod(k:k+p-1,l:l+q-1)=A(i,j)*B
	end do
end do

end function kronprod
! ---------------------------------------------


end module alexutils