!######################################################################
! Module : dos
! Version: v0.0
! Author : Q. X. & J. X. Li
! Date   : 2018-12-15
! Content: doscar related
!######################################################################
module dos
use utilities, only : numofstr, read_nline
use gnu
implicit none

contains

!
!   %--------------%
!      read_dos
!   %--------------%
!
subroutine read_doscar(fermi)
! fermi energy from scf calculation
real*8, intent(in) :: fermi

character(len=512) :: s
character(len=79) :: ss
character(len=4) :: ch

real*8 :: fermi_dos
integer :: ISPIN
integer :: nedos, natom
integer :: i, n

logical :: alive

!
! START
!
inquire(file="DOSCAR",exist=alive)
if (.not. alive) then
write(6,"(a)") "Warning: DOSCAR doesn't exist!!!"
return
end if

open(unit=11, file='DOSCAR', status='old')
read(11,*) natom
do i = 1, 4
read(11,*)
enddo
read(11,'(a79)') ss
read(ss(33:40),*) nedos
read(ss(41:54),*) fermi_dos

write(6,"(a,f8.4)") "scf E-Fermi = ", fermi
write(6,"(a,f8.4)") "dos E-Fermi = ", fermi_dos

open(unit=12,file='dos.dat',status='replace')
write(6,"(a)") 'Write total density to dos.dat'
do i = 1, nedos
read(11,'(a512)') s
write(12,'(a)') trim(adjustl(s))
enddo
close(12)

n = numofstr(s) - 1
ISPIN = n/2  ! occupation
call write_gnu_dos(fermi, ISPIN)
end subroutine



!
!     %---------------------------------%
!               split doscar
!     %---------------------------------%
!
subroutine split_doscar( fermi )
! fermi energy from scf calculation
real*8, intent(in) :: fermi

character(len=512) :: s
character(len=79) :: ss
character(len=4) :: ch

real*8 :: fermi_dos

! Total density for each atom
real*8, allocatable :: density(:)

! number of lines of DOSCAR file
integer :: NLdos

! spin polarized or not
integer :: ISPIN

integer :: NProj
real*8 :: RProj

! number of projected orbitals
integer :: natom, nedos, nobt

! energy
real*8 :: e

integer :: i, j, n

logical :: alive

!
! START
!
inquire(file="DOSCAR",exist=alive)
if (.not. alive) then
write(6,"(a)") "Warning: DOSCAR doesn't exist!!!"
return
end if

NLdos = read_nline("DOSCAR")

open(unit=11, file='DOSCAR', status='old')
read(11,*) natom
do i = 1, 4
read(11,*)
enddo
read(11,'(a79)') ss
read(ss(33:40),*) nedos
read(ss(41:54),*) fermi_dos

write(6,"(a,f8.4)") "scf E-Fermi = ", fermi
write(6,"(a,f8.4)") "dos E-Fermi = ", fermi_dos

open(unit=12,file='dos.dat',status='replace')
write(6,"(a)") 'Write total density to dos.dat'
do i = 1, nedos
read(11,'(a512)') s
write(12,'(a)') trim(adjustl(s))
enddo
close(12)

n = numofstr(s) - 1
ISPIN = n/2  ! occupation
call write_gnu_dos(fermi, ISPIN)

!
! project DOS
!
RProj = dble(NLdos - 5)/dble(nedos + 1)

if ( abs(RProj - nint(RProj)) > 1e-6 ) then
write(6,"(a)") "Error: Wrong number of line in DOSCAR!!!"
return
end if

NProj = nint(RProj)
if (NProj == natom + 1) then
write(6,"(a)") "Write projected density files"
else if (NProj == 1) then
close(11)
return
else
write(6,"(a)") "Warning: Wrong number of line in DOSCAR file."
close(11)
return
end if

do i = 1, natom
write(ch,'(i4)') i
write(6,"(a)") "Write projected density to dos"//trim(adjustl(ch))//'.dat'
open(unit=12,file='dos'//trim(adjustl(ch))//'.dat',status='replace')
read(11,*) ! skip the energy line

if (i == 1) then
read(11,'(a512)') s
nobt = numofstr(s) - 1
backspace(11)
allocate(density(nobt))
end if

do j = 1, nedos
read(11,'(a512)') s
read(s,*) e, density(1:nobt)
write(12,'(a,e12.4)') trim(s)//'  ', sum(density(:))
enddo

close(12)
enddo


Write(6,"(a,i4)") "nobt  = ", nobt
Write(6,"(a,i4)") "ISPIN = ", ISPIN


! also implement the orbital name later!!!

call  write_gnu_projdos(fermi, ISPIN, nobt, natom)

if(allocated(density)) deallocate(density)

end subroutine


!
!    %---------------------%
!         write_gnu_dos
!    %---------------------%
!
subroutine write_gnu_dos(fermi, ISPIN)
real*8, intent(in) :: fermi
integer, intent(in) :: ISPIN

write(6,"(a)") "Write gnuplot script ---> plotdos.gnu"
open(unit=25,file="plotdos.gnu",status="unknown")
call write_gnu_terminal(25, "dos", 10)
call write_gnu_linestyle(25, 10)
write(25,"(a)")
write(25,"(a)") "set xlabel 'E - E_F [eV]'"
write(25,"(a)") "set ylabel 'DOS (1/Cell)'"
write(25,"(a)") "set xrange [-2: 2]"
write(25,"(a)") "#set yrange [-2: 2]"
write(25,"(a)") "#set xtics 1"
write(25,"(a)") "#set ytics 1"
write(25,"(a)")
write(25,"(a,f8.4)") "fermi = ", fermi
write(25,"(a)")
write(25,"(a)") "file = 'dos.dat'"
write(25,"(a)")
if (ISPIN == 1) then
write(25,"(a)") "plot file u ($1-fermi):2 w l ls 3 notitle"
else
write(25,"(a)") "plot \"
write(25,"(a)") "file u ($1-fermi): ($2) w l ls 3 title 'spin up',\"
write(25,"(a)") "file u ($1-fermi):(-$3) w l ls 4 title 'spin down'"
end if
write(25,"(a)")
write(25,"(a)")
close(25)
end subroutine


!
!    %------------------------------%
!         write_gnu_projdos
!    %------------------------------%
!
subroutine write_gnu_projdos(fermi, ISPIN, nobt, natom)
real*8, intent(in) :: fermi
integer, intent(in) :: ISPIN
! number of projected functions s p d f ...
integer, intent(in) :: nobt
integer, intent(in) :: natom

character(len=4) :: ch
integer :: i, j

do i = 1, natom
write(ch,"(i4)") i
write(6,"(a)") 'Write gnu script to ---> plotdos'//trim(adjustl(ch))//'.gnu'
open(unit=25, file='plotdos'//trim(adjustl(ch))//'.gnu', status='unknown')
call write_gnu_terminal(25, "dos"//trim(adjustl(ch)), 10)
call write_gnu_linestyle(25, nobt+10)
write(25,"(a)")
write(25,"(a)") "set xlabel 'E - E_F [eV]'"
write(25,"(a)") "set ylabel 'DOS (1/Cell)'"
write(25,"(a)") "set xrange [-2 : 2]"
write(25,"(a)") "#set yrange [0 : 100]"
write(25,"(a)") "#set xtics 1"
write(25,"(a)") "#set ytics 1"
write(25,"(a)")
write(25,"(a)") "file = 'dos"//trim(adjustl(ch))//".dat'"
write(25,"(a)")
write(25,"(a,f8.4)") "fermi = ", fermi
write(25,"(a)")
write(25,"(a,i4,a)") "plot \"

do j = 1, nobt-1
write(ch,"(i4)") j+1
write(25,"(a,i4,a)") "file u ($1-fermi):($"//trim(adjustl(ch))//") w l ls ", j+2, " notitle ,\"
enddo

write(ch,"(i4)") nobt+1
write(25,"(a,i4,a)") "file u ($1-fermi):($"//trim(adjustl(ch))//") w l ls ", nobt+2, " notitle"
write(25,"(a)")
write(25,"(a)")
close(25)
enddo


! Write a multiplot script here

end subroutine

end module


