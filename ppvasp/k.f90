!######################################################################
! Module : kpt
! Version: v0.0
! Author : Q. X. & J. X. Li
! Date   : 2018-12-15
! Content: KPOINTS related
!######################################################################
module kpt
use utilities, only : ifile, is_file_exist
use vector, only : norm, dot
implicit none


! kpts
integer :: nkpt = 11
real*8, allocatable :: kpath(:,:,:)  ! n,nline,2
character(len=12), allocatable :: kname(:,:)  ! 2,nline

! n line mode
integer :: nline
real*8, allocatable :: lmode(:)  ! nline
real*8, allocatable :: lmodes(:) ! nline + 1


contains

!
!     %------------------------------------------%
!                    read_kpath
!     %------------------------------------------%
!
! Notice that Kpath in cart
subroutine read_kpath(tag,n,a,b,nline,kname,kpath)
character(len=*), intent(in) :: tag

! 2 or 3
integer, intent(in) :: n
real*8, intent(in) :: a(n,n), b(n,n)

integer, intent(out) :: nline
real*8, allocatable, intent(out) :: kpath(:,:,:)
character(len=12), allocatable, intent(out) :: kname(:,:)

real*8 :: v1(n), v2(n), v3(n), v4(n)
character(len=79) :: s
integer :: i, info
logical :: found

call is_file_exist(ifile)

!write(6,"(a)") " Reading "//trim(adjustl(tag))//" from "//trim(ifile)
open(unit=10,file=ifile,status="old")
found = .false.
do while(.true.)
read(unit=10,fmt=*,iostat=info) s
if (info /= 0) exit
if ( s == tag ) then
read(10,*) nline

allocate(kpath(n,nline,2), kname(2,nline))
do i = 1, nline
read(10,*) kname(1,i), v1, kname(2,i), v2
call dirt2cart_K(n,a,b,v1,v3)
call dirt2cart_K(n,a,b,v2,v4)
kpath(1:n,i,1) = v3
kpath(1:n,i,2) = v4
enddo
found = .true.
exit
end if
enddo
close(10)

if (.not. found) then
    write(6,"(a)") " Error: "//trim(adjustl(tag))//" not found in "//trim(ifile)
stop
end if
end subroutine


!
!     %------------------------------------------%
!                     write_lmodes
!     %------------------------------------------%
! Notice that Kpath in cart
subroutine write_kpath(n,nline,kname,kpath)
integer, intent(in) :: n, nline
real*8, intent(in) :: kpath(n,nline,2)
character(len=*) :: kname(2,nline)
integer :: i, j

write(*,"(a,i4)") " nline = ", nline
do i = 1, nline
!write(*,*) kname(1,i), kpath(:,i,1), kname(2,i), kpath(:,i,2)

write(unit=6,fmt="(a)",advance="no") kname(1,i)
do j = 1, n
write(unit=6,fmt="(f8.4)",advance="no") kpath(j,i,1)
enddo
write(unit=6,fmt="(a)",advance="no") kname(2,i)
do j = 1, n
write(unit=6,fmt="(f8.4)",advance="no") kpath(j,i,2)
enddo
write(6,"(a)")
enddo
end subroutine


!
!     %------------------------------------------%
!                     get_lmodes
!     %------------------------------------------%
! Notice that Kpath in cart
subroutine get_lmodes(n,nline,kpath,lmode,lmodes)
integer, intent(in) :: n, nline
real*8, intent(in) :: kpath(n,nline,2)
real*8, allocatable, intent(out) :: lmode(:), lmodes(:)

real*8 :: v1(n), v2(n)
integer :: i

allocate(lmode(nline), lmodes(nline+1))
lmode(:) = 0.0
do i = 1, nline
lmode(i) = norm(kpath(:,i,1) - kpath(:,i,2))
enddo

lmodes(1) = 0.d0
do i=2,nline+1
lmodes(i) = lmodes(i-1) + lmode(i-1)
enddo
end subroutine

!
!     %------------------------------------------%
!                     write_lmodes
!     %------------------------------------------%
! Notice that Kpath in cart
subroutine write_lmodes(n,nline,lmode,lmodes)
integer, intent(in) :: n, nline
real*8, intent(in) :: lmode(nline), lmodes(nline+1)
integer i
if ( n== 2) then
write(6,"(a)") " lmode2d --->"
else
write(6,"(a)") " lmode --->"
end if
do i = 1, nline
write(unit=6,fmt="(f8.4)",advance="no") lmode(i)
enddo
write(unit=6,fmt="(f8.4)",advance="yes")

if ( n== 2) then
write(6,"(a)") " lmodes2d --->"
else
write(6,"(a)") " lmodes --->"
end if
do i = 1, nline+1
write(unit=6,fmt="(f8.4)",advance="no") lmodes(i)
enddo
write(unit=6,fmt="(f8.4)",advance="yes")

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

coef = dot(a(:,1),b(:,1))
a_t = transpose(a)
v = 0.0
do i = 1, n
v = v + Kc(i)*a_t(:,i)
enddo
Kd = v / coef
end subroutine

end module


