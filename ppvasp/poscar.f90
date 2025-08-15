!######################################################################
! Module : poscar
! Version: v0.0
! Author : Q. X. & J. X. Li
! Date   : 2018-12-15
! Content: poscar related
!######################################################################
module poscar
use const
use vector
use gnu
implicit none

contains

!
!     %------------------%
!        read_POSCAR
!     %------------------%
!
subroutine read_poscar(a,b)
real*8, intent(out) :: a(3,3), b(3,3)
real*8 :: sf ! scale factor
logical :: alive

inquire(file="POSCAR",exist=alive)
if (.not. alive ) then
write(6,"(a)") "POSCAR file doesn't exist!!!"
end if
open(unit=11,file="POSCAR",status="old")
read(11,*)
read(11,*) sf
read(11,*) a(:,1)
read(11,*) a(:,2)
read(11,*) a(:,3)
a = sf * a
call reciprocal_3d(a,b)
end subroutine

!
!     %------------------%
!        reciprocal_2d
!     %------------------%
!      a(1:2,1) the 1st
!      a(1:2,2) the 2nd
subroutine reciprocal_2d(a,b)
real*8, intent(in) :: a(2,2)
real*8, intent(out) :: b(2,2)
real*8 :: v, coef
coef = reciprocal_scale
v = a(1,1) * a(2,2) - a(2,1) * a(1,2)
b(1,1) =   coef * a(2,2) / v
b(2,1) = - coef * a(1,2) / v
b(1,2) = - coef * a(2,1) / v
b(2,2) =   coef * a(1,1) / v
end subroutine

!
!     %------------------%
!        reciprocal_3d
!     %------------------%
!      a(1:3,1) the 1st
!      a(1:3,2) the 2nd
!      a(1:3,3) the 3rd
subroutine reciprocal_3d(a,b)
real*8, intent(in) :: a(3,3)
real*8, intent(out) :: b(3,3)
real*8 :: v, coef
coef = reciprocal_scale
v = dot(cross(a(:,1),a(:,2)),a(:,3))
b(:,1) = coef * cross(a(:,2), a(:,3)) / v
b(:,2) = coef * cross(a(:,3), a(:,1)) / v
b(:,3) = coef * cross(a(:,1), a(:,2)) / v
end subroutine

end module

