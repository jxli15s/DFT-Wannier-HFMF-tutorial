!######################################################################
! Module : ppvasp
! Version: v0.0
! Author : Q. X. & J. X. Li
! Date   : 2018-12-15
! Content: post-process vasp
!######################################################################
program main
use utilities
use dos
use poscar
use kpt
use eigenval
use procar
implicit none


real*8 :: a(3,3), b(3,3)

! fermi energy
real*8 :: fermi = 0.0

integer :: Imode

!
! S. T. A. R. T
!
call sayHi
call date_time

1000 continue
write(6,"(a)") "[1]  dos"
write(6,"(a)") "[2]  proj dos"

write(6,"(a)") "[3]  band along K path"
write(6,"(a)") "[4]  proj band"
write(6,"(a)") "[5]  band on K mesh"

write(unit=6,fmt="(a)",advance="no") "Please choose a calculation mode --> "
read(5,*) Imode
if (Imode < 1 .or. Imode > 5) then
write(6,"(a)") "Warning: Calculation mode not implemented!!!"
goto 1000
end if

call get_fermi_energy(fermi)

if (Imode == 1) call read_doscar(fermi)
if (Imode == 2) call split_doscar(fermi)

if (Imode == 3) call read_eigenval_line_mode(fermi)
if (Imode == 4) call read_procar(fermi)
if (Imode == 5) call read_eigenval_mesh_mode(fermi)

stop
end program
