!######################################################################
! Module : kpt
! Version: v0.0
! Author : Q. X. & J. X. Li
! Date   : 2018-12-15
! Content: KPOINTS related
!######################################################################
module kpt
use const
use vector, only : norm
use utilities, only : read_nline
implicit none

integer :: nkpt

real*8, allocatable :: kpath(:,:,:)
character(len=30), allocatable :: kname(:,:)

! n line mode
integer nmode
real*8, allocatable :: lmode(:), lmodes(:)

contains


!
!   %------------------%
!      read_KPOINTS
!   %------------------%
!
subroutine read_KPOINTS(a,b,nkpt,nmode,kpath)
real*8, intent(in) :: a(3,3), b(3,3)
integer, intent(out) :: nkpt, nmode
real*8, allocatable, intent(out) :: kpath(:,:,:)

character(len=1) :: ch, coor
real*8 :: v, v1(3), v2(3)
integer :: i, icount, info
logical :: alive

!
! START
!
inquire(file="KPOINTS",exist=alive)
if (.not.alive) then
write(6,"(a)") "Warning: KPOINTS doesn't exist!!!"
stop
end if

open(unit=11,file="KPOINTS",status="old")
read(11,*)
read(11,*) nkpt
read(11,*) ch

if (ch /= 'L') then
write(6,"(a)") "Warning: KPOINTS not in Line mode"
end if

read(11,*) coor

icount = 0
do while (.true.)
read(unit=11,fmt=*,iostat=info) v
if (info /= 0) exit
icount = icount + 1
enddo
nmode = icount/2

rewind(11)
do i=1,4
read(11,*)
enddo

! kpath always in cart
allocate(kpath(3,nmode,2))
do i = 1, nmode
read(11,*) v1
read(11,*) v2
if ( coor == 'K' .or. coor == 'k' .or. &
&     coor == 'C' .or. coor == 'c') then
kpath(:,i,1) = v1
kpath(:,i,2) = v2
else
! Direct
kpath(:,i,1) = v1(1)*b(1:3,1) + v1(2)*b(1:3,2) + v1(3)*b(1:3,3)
kpath(:,i,2) = v2(1)*b(1:3,1) + v2(2)*b(1:3,2) + v2(3)*b(1:3,3)
end if
enddo

kpath(:,:,:) = kpath(:,:,:)*2.0*pi/reciprocal_scale


!! Q.X. comment
!do i = 1, nmode
!v1 = kpath(:,i,1)
!v2 = kpath(:,i,2)
!write(*,"(3f12.6,4x,3f12.6,f12.6)") v1, v2, norm(v1-v2)
!enddo


close(11)
end subroutine


!
!   %------------------%
!       get_lmoes
!   %------------------%
!
subroutine get_lmodes(nkpt,nmode,kpath,lmode,lmodes)
integer, intent(in) :: nkpt, nmode
real*8, intent(in) :: kpath(3,nmode,2)
real*8, allocatable, intent(out) :: lmode(:), lmodes(:)
integer :: i

allocate(lmode(nmode), lmodes(nmode))
lmode(:) = 0.0
do i = 1, nmode
lmode(i) = norm(kpath(:,i,1) - kpath(:,i,2))
enddo

lmodes(1) = 0.d0
do i=2,nmode
lmodes(i) = lmodes(i-1) + lmode(i-1)
enddo
end subroutine





!
!     %------------------------------------------%
!                    dirt2cart_K
!     %------------------------------------------%
!      n the dimension
!      a b the lattices
!      Kd Kc k points in direct and cart
subroutine dirt2cart_K(n,a,b,Kd,Kc)
integer, intent(in) :: n
real*8, intent(in) :: a(n,n), b(n,n)
real*8, intent(in) :: Kd(n)
real*8, intent(out) :: Kc(n)
real*8 :: coef
integer i
Kc = 0.0
do i = 1, n
Kc = Kc + Kd(i)*b(:,i)
enddo
end subroutine


!
!     %------------------------------------------%
!                    cart2dirt_K
!     %------------------------------------------%
! something wrong here
subroutine cart2dirt_K(n,a,b,Kc,Kd)
integer, intent(in) :: n
real*8, intent(in) :: a(n,n), b(n,n)
real*8, intent(in) :: Kc(n)
real*8, intent(out) :: Kd(n)
real*8 :: a_t(n,n), v(n), coef
integer i

coef = 0.0
do i = 1, n 
coef = coef + a(i,1)*b(i,1)
enddo

a_t = transpose(a)
v = 0.0
do i = 1, n
v = v + Kc(i)*a_t(:,i)
enddo
Kd = v / coef
end subroutine



end module
