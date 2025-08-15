!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Module : vector (checked)
! Content: basic subroutines and functions of vectors
! Author : Q. X. & J. X. Li
! Date   : 2016-11-01
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
module vector
implicit none

!
! List of subroutines and functions:
! 1. dot
! 2. inner
! 3. norm
!
interface dot
module procedure dot_I
module procedure dot_R
end interface

interface norm
module procedure norm_vector_I
module procedure norm_vector_R
module procedure norm_vector_C
module procedure norm_matrix_I
module procedure norm_matrix_R
module procedure norm_matrix_C
end interface

interface random_vector
module procedure random_real_vector
module procedure random_complex_vector
end interface


contains

!
!     %---------%
!        dot_I
!     %---------%
!
real*8 function dot_I(a, b) result (ans)
integer, intent(in) :: a(:), b(:)
integer :: n, na, nb
integer :: s
integer :: i
na = size(a)
nb = size(b)

n = min(na, nb)
s = 0
do i = 1, n
s = s + a(i) * b(i)
enddo
ans = dble(s)
end function
!
!     %---------%
!        dot_R
!     %---------%
!
real*8 function dot_R(a, b) result (ans)
real*8, intent(in) :: a(:), b(:)
integer :: n, na, nb
real*8 :: s
integer :: i
na = size(a)
nb = size(b)

n = min(na, nb)
s = 0.0
do i = 1, n
s = s + a(i) * b(i)
enddo
ans = s
end function
!
!     %---------%
!        inner
!     %---------%
!
complex*16 function inner(a, b) result (ans)
complex*16, intent(in) :: a(:), b(:)
integer :: n, na, nb
complex*16 :: z
integer :: i
na = size(a)
nb = size(b)

n = min(na, nb)
z = 0.0
do i = 1, n
z = z + conjg(a(i)) * b(i)
enddo
ans = z
end function
!
!     %-------------------%
!        norm for vector
!     %-------------------%
!
real*8 function norm_vector_I(a) result(ans)
integer, intent(in) :: a(:)
integer :: n
integer :: s
integer :: i
n = size(a)
s = 0
do i = 1, n
s = s + a(i) * a(i)
enddo
ans = dsqrt(dble(s))
end function

real*8 function norm_vector_R(a) result(ans)
real*8, intent(in) :: a(:)
integer :: n
real*8 :: s
integer :: i
n = size(a)
s = 0.0
do i = 1, n
s = s + a(i) * a(i)
enddo
ans = dsqrt(s)
end function

real*8 function norm_vector_C(a) result(ans)
complex*16, intent(in) :: a(:)
integer :: n
complex*16 :: z
integer :: i
n = size(a)
z = 0.0
do i = 1, n
z = z + conjg(a(i)) * a(i)
enddo
ans = dsqrt(dble(z))
end function
!
!     %-------------------%
!        norm for matrix
!     %-------------------%
!
real*8 function norm_matrix_I(a) result(ans)
integer, intent(in) :: a(:,:)
integer :: m, n
integer :: s
integer :: i, j
m = size(a, 1)
n = size(a, 2)
s = 0
do j = 1, n
do i = 1, m
s = s + a(i,j) * a(i,j)
enddo
enddo
ans = dsqrt(dble(s))
end function

real*8 function norm_matrix_R(a) result(ans)
real*8, intent(in) :: a(:,:)
integer :: m, n
real*8 :: s
integer :: i, j
m = size(a, 1)
n = size(a, 2)
s = 0.0
do j = 1, n
do i = 1, m
s = s + a(i,j) * a(i,j)
enddo
enddo
ans = dsqrt(s)
end function

real*8 function norm_matrix_C(a) result(ans)
complex*16, intent(in) :: a(:,:)
integer :: m, n
complex*16 :: z
integer i, j
m = size(a, 1)
n = size(a, 2)
z = 0.0
do j = 1, n
do i = 1, m
z = z + conjg(a(i,j)) * a(i,j)
enddo
enddo
ans = dsqrt(dble(z))
end function

!
!     %---------%
!        cross
!     %---------%
!
function cross(a,b)
real*8, intent(in) :: a(3), b(3)
real*8 :: cross(3)
cross(1) = a(2) * b(3) - a(3) * b(2)
cross(2) = a(3) * b(1) - a(1) * b(3)
cross(3) = a(1) * b(2) - a(2) * b(1)
end function

!                                                   !
!---------------------------------------------------!
! generate a random complex*16 vector with norm 1   !
!   use another random number generator (RNG)       !
!---------------------------------------------------!
!                                                   !
subroutine random_complex_vector(n, v)
integer :: i, n
!real*8 :: v(n)
complex*16 :: v(n)
!real*8, external :: norm
real*8 r, s, rs
call random_seed
s = 0.d0
do i = 1, n
call random_number(r)
s = s + r*r
v(i) = dcmplx(r)
enddo
!v = v/norm(n, v)
rs = sqrt(s)
do i = 1, n
v(i) = v(i) / rs
enddo
return
end subroutine
!                                                  !
!--------------------------------------------------!
! generate a random complex*16 vector with norm 1  !
!   use another random number generator (RNG)      !
!--------------------------------------------------!
!                                                  !
subroutine random_real_vector(n, v)
integer :: i, n
real*8 :: v(n)
!complex*16 :: v(n)
!real*8, external :: norm
real*8 r, s, rs
call random_seed
s = 0.d0
do i = 1, n
call random_number(r)
s = s + r*r
v(i) = r
enddo
!v = v/norm(n, v)
rs = sqrt(s)
do i = 1, n
v(i) = v(i) / rs
enddo
return
end subroutine

end module

