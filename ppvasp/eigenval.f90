!######################################################################
! Module : eigenval
! Version: v0.0
! Author : Q. X. & J. X. Li
! Date   : 2018-12-15
! Content: EIGENVAL related
!######################################################################
module eigenval
use utilities, only : numofstr, get_fermi_energy
use poscar
use kpt
use gnu
implicit none

contains

!
!    %---------------------------%
!       read eigenval line mode
!    %---------------------------%
!
subroutine read_eigenval_line_mode(fermi)
character(len=79) :: s
real*8, allocatable :: e(:,:,:,:)
real*8 :: a(3,3), b(3,3), kc(3), kd(3)
integer :: ISPIN
real*8 :: fermi
integer :: tem61
integer :: nkpt, mkpt, nbnd
real*8 :: x
integer i, j, k, l, ibnd
logical alive

!
! START
!
call read_POSCAR(a,b)
call read_KPOINTS(a,b,nkpt,nmode,kpath)
call get_lmodes(nkpt,nmode,kpath,lmode,lmodes)

inquire(file="EIGENVAL",exist=alive)
if (.not.alive) then
write(6,"(a)") "Warning: EIGENVAL doesn't exist!!!"
stop
end if

!Read header of EIGENVAL
open(unit=12, file='EIGENVAL', status='old')
do i=1,5
read(12,*)
enddo
read(12,*) tem61, nkpt, nbnd

mkpt = (nkpt / nmode)

read(12,*)
read(12,*)
read(12,"(a79)") s
ISPIN = (numofstr(s) - 1)/2

backspace(12)
backspace(12)
backspace(12)

!Read EIGENVAL
allocate( e(ISPIN, nmode, mkpt, nbnd) )
do l=1,nmode
do k=1,mkpt
read(12,*)
read(12,*)
do j=1,nbnd
read(12,*) ibnd, e(:,l,k,j)
enddo
enddo
enddo
close(12)


write(6,"(a)") "Write bands to bands.dat"
open(unit=13, file='bands.dat', status='replace')
!if (ISPIN == 1) write(13,'(a,a)') "#       x              E               ", &
!& "kx          ky          kz          k1          k2          k3"
!if (ISPIN == 2) write(13,'(a,a)') "#       x             Eup         Edn              ",&
!& "kx          ky          kz          k1          k2          k3"

do j=1,nbnd
!write(13,"(a,i4)") "#bands : ", j
do l=1,nmode
do k=1,mkpt
x = lmodes(l) + (k-1) * lmode(l)/(mkpt-1)

kc = kpath(:,l,1) + (k-1)*(kpath(:,l,2) - kpath(:,l,1))/(mkpt-1)
call cart2dirt_K(3,a,b,kc,kd)

if (ISPIN == 1) write(13,'(f12.6,4x, f12.6)') x, e(1:1,l,k,j)
if (ISPIN == 2) write(13,'(f12.6,4x,2f12.6)') x, e(1:2,l,k,j)

!if (ISPIN == 1) write(13,'(f12.6,4x, f12.6,4x,6f12.6)') x, e(1:1,l,k,j), kc, kd
!if (ISPIN == 2) write(13,'(f12.6,4x,2f12.6,4x,6f12.6)') x, e(1:2,l,k,j), kc, kd

enddo
enddo
write(13,'(a)')
enddo
close(13)

call write_gnu_band_line_mode(ISPIN,fermi,mkpt,nmode,nbnd,lmode,lmodes,e)

deallocate(lmode,lmodes)
deallocate(e)
end subroutine

!
!    %---------------------------%
!       read eigenval mesh mode
!    %---------------------------%
!
subroutine read_eigenval_mesh_mode(fermi)
real*8, intent(in) :: fermi

character(len=79) :: s
character(len=4) :: ch

real*8, allocatable :: e(:,:,:,:)

integer :: ISPIN
integer :: tem61
integer :: nkpt, mkpt, nbnd

real*8 :: kv(3), deltaK(3)

real*8 :: a(3,3), b(3,3)

integer i, j, k, l, ibnd, icount, nx

integer, allocatable :: xbnd(:)

logical alive

call read_POSCAR(a,b)
call read_KPOINTS(a,b,nkpt,nmode,kpath)

inquire(file="EIGENVAL",exist=alive)
if (.not.alive) then
write(6,"(a)") "Warning: EIGENVAL doesn't exist!!!"
stop
end if

!Read header of EIGENVAL
open(unit=12, file='EIGENVAL', status='old')
do i=1,5
read(12,*)
enddo
read(12,*) tem61, nkpt, nbnd

mkpt = (nkpt / nmode)

read(12,*)
read(12,*)
read(12,"(a79)") s
ISPIN = (numofstr(s) - 1)/2

backspace(12)
backspace(12)
backspace(12)

!Read EIGENVAL
allocate( e(ISPIN, nmode, mkpt, nbnd) )
do l=1,nmode
do k=1,mkpt
read(12,*)
read(12,*)
do j=1,nbnd
read(12,*) ibnd, e(:,l,k,j)
enddo
enddo
enddo
close(12)


do j=1,nbnd
write(ch,'(i4)') j
write(6,"(a)") "Write bands to band"//trim(adjustl(ch))//".dat"
open(unit=13, file='band'//trim(adjustl(ch))//'.dat', status='unknown')

do l=1,nmode
deltaK(:) = (kpath(:,l,2) - kpath(:,l,1)) / (mkpt-1)
do k=1,mkpt
kv(:) = kpath(:,l,1) + (k-1) * deltaK(:)
!call cart2dirt_K(3,a,b,kv,kd)
if (ISPIN == 1) write(13,'(3f12.6,4x, f12.6)') kv, e(1:1,l,k,j)
if (ISPIN == 2) write(13,'(3f12.6,4x,2f12.6)') kv, e(1:2,l,k,j)
enddo
write(13,'(a)')
enddo
close(13)
enddo

!
! get the band index that cross the E_F
!
icount = 0
do j = 1, nbnd
if (maxval(e(:,:,:,j)) > fermi &
&   .and. minval(e(:,:,:,j)) < fermi) then
write(6,"(a,i3,a)") "Band ", j, " cross the fermi level"
icount = icount + 1
end if
enddo

if (icount == 0) then

nx = 2
allocate(xbnd(nx))
loop: do j = 1, nbnd
if (maxval(e(:,:,:,j)) < fermi &
&   .and. minval(e(:,:,:,j+1)) > fermi) then
xbnd(1) = j
xbnd(2) = j + 1
write(6,"(a)") "We have a band gap on the band structure."
write(6,"(a,2i3)") "Conduct and Valance bands are", j, j+1
exit loop
end if
enddo loop

else

nx = icount
allocate(xbnd(nx))
icount = 0
do j = 1, nbnd
if (maxval(e(:,:,:,j)) > fermi &
&   .and. minval(e(:,:,:,j)) < fermi) then
icount = icount + 1
xbnd(icount) = j
end if
enddo
end if

call write_gnu_band_mesh_mode(fermi,ISPIN,nx,xbnd)

deallocate(e)
deallocate(xbnd)
deallocate(kpath)
end subroutine




!
!    %----------------------%
!         write_gnu_band
!    %----------------------%
!
subroutine write_gnu_band_line_mode(ISPIN,fermi,mkpt,nmode,nbnd,lmode,lmodes,e)
integer, intent(in) :: ISPIN, mkpt, nmode, nbnd
real*8, intent(in) :: fermi, lmode(nmode), lmodes(nmode)
real*8, intent(in) :: e(ISPIN,nmode,mkpt,nbnd)
real*8 :: emin, emax, xmax
integer :: i

write(6,"(a)") "Write gnuplot script ---> plotband.gnu"
open(unit=25, file='plotband.gnu', status='unknown')
call write_gnu_terminal(25,'band',20)
call write_gnu_linestyle(25, 20)
write(25,"(a)")
emin = minval( e(1:ispin,1:nmode,1:mkpt,1) )
emax = maxval( e(1:ispin,1:nmode,1:mkpt,nbnd) )

if (nmode >= 2) then
write(25,'(a)') "$vl << EOF"
do i=2,nmode
write(25,'(2f12.6)') lmodes(i), real(ceiling(emax-fermi+5.0))
write(25,'(2f12.6)') lmodes(i), 0.0
write(25,'(a)')
write(25,'(2f12.6)') lmodes(i), 0.0
write(25,'(2f12.6)') lmodes(i), real(ceiling(emin-fermi-5.0))
if (i /= nmode) write(25,'(a)')
enddo
write(25,'(a)') "EOF"
end if
write(25,"(a)")
write(25,'(a)') "$fl << EOF"
xmax = lmodes(nmode) + lmode(nmode)
write(25,'(2f12.6)') 0.0, 0.0
write(25,'(2f12.6)') xmax, 0.0
write(25,'(a)') "EOF"
write(25,'(a)')
write(25,'(a,f12.6,a,f12.6,a)') &
&   'set xrange [',  0.0, ' :', xmax, ' ]'
write(25,'(a,f12.2,a,f12.2,a)') &
&   'set yrange [', emin-fermi, ' :', emax-fermi, ' ]'
write(25,'(a,f12.2,a,f12.2,a)') &
&   '#set yrange [', -5., ' :', 5., ' ]'
write(25,'(a,f12.2,a,f12.2,a)') &
&   '#set yrange [', -2., ' :', 2., ' ]'

write(25,'(a)')
write(25,'(a)') "set ylabel 'E - E_F [eV]'"

write(unit=25,fmt='(a)') 'set xtics (  \'
do i=1,nmode
write(25,'(a,f12.6,a)') ' "{/Symbol G}" ', lmodes(i), '   ,\'
enddo
write(25,'(a,f12.6,a)') ' "{/Symbol G}" ', xmax, '   )'
write(25,'(a)') '#set ytics 2'
write(25,'(a)')
write(25,"(a,f8.4)") "fermi = ", fermi
write(25,"(a)") "file = 'bands.dat'"
write(25,"(a)") "plot \"
if (nmode >= 2 ) write(25,'(a)') '$vl w l ls 47 dt (10,7) notitle ,\'
write(25,'(a)') '$fl w l ls 47 notitle ,\'
if (ISPIN == 1) then
write(25,"(a)") "file u 1:($2-fermi) w l ls 3 notitle"
else if (ISPIN == 2) then
write(25,"(a)") "file u 1:($2-fermi) w l ls 3 title 'spin up',\"
write(25,"(a)") "file u 1:($3-fermi) w l ls 4 title 'spin down'"
end if
write(25,"(a)")
write(25,"(a)")
close(25)
end subroutine


!
!   %-------------------------------%
!        write_gnu_band_mesh_mode
!   %-------------------------------%
!
subroutine write_gnu_band_mesh_mode(fermi,ISPIN,nx,xbnd )
real*8, intent(in) :: fermi
integer, intent(in) :: ISPIN
integer, intent(in) :: nx
integer, intent(in) :: xbnd(nx)
character(len=4) :: ch
integer :: i

write(6,"(a)") "Wrinte gnuplot script ---> plotbandmesh.gnu"
open(unit=25,file="plotbandmesh.gnu",status="unknown")
call write_gnu_terminal(25,"bandmesh",10)
call write_gnu_linestyle(25,20)
write(25,'(a)')
write(25,'(a)')
write(25,'(a)') "set border linewidth 2"
write(25,'(a)') "set border 4095"
write(25,'(a)') "unset key"
write(25,'(a)') 'set view 80, 30'
write(25,'(a)') 'set hidden3d'
write(25,'(a)') '#set xyplane at -2.0'
write(25,'(a)') 'set xlabel "k_x"'
write(25,'(a)') 'set ylabel "k_y"'
write(25,'(a)') 'set zlabel "E - E_F [eV]" rotate by 90'
write(25,'(a)')
write(25,'(a)') '#set xtics 0.1'
write(25,'(a)') '#set ytics 0.1'
write(25,'(a)') '#set ztics 0.5'
write(25,'(a)')
write(25,'(a)') "set palette defined ( 0 'blue', 1 'white', 2 'red' )"
write(25,'(a)') "#set cbrange [ xxx : xxx ]"
write(25,'(a)') "#set format cb '%.0s%c'"
write(25,'(a)') "#set cbtics 0.2"
write(25,'(a)') "#unset colorbox"
write(25,'(a)')
write(25,'(a)') "set pm3d depth lighting"
write(25,'(a)') "set style fill transparent solid 0.8 noborder"
write(25,'(a)') "set pm3d interpolate 0, 0"
write(25,'(a)')
write(25,'(a,f8.4)') "fermi = ", fermi
write(25,'(a)')
if (ISPIN == 1) then
write(25,'(a)') 'splot \'
do i = 1, nx - 1
write(ch,"(i4)") xbnd(i)
write(25,'(a)') '"band'//trim(adjustl(ch))//'.dat" u 1:2:($4-fermi) with pm3d ,\'
enddo
write(ch,"(i4)") xbnd(nx)
write(25,'(a)') '"band'//trim(adjustl(ch))//'.dat" u 1:2:($4-fermi) with pm3d '

else

write(25,'(a)') 'splot \'
do i = 1, nx
write(ch,"(i4)") xbnd(i)
write(25,'(a)') '"band'//trim(adjustl(ch))//'.dat" u 1:2:($4-fermi) with pm3d title "spin up",\'
enddo
do i = 1, nx - 1
write(ch,"(i4)") xbnd(i)
write(25,'(a)') '"band'//trim(adjustl(ch))//'.dat" u 1:2:($5-fermi) with pm3d title "spin down",\'
enddo
write(ch,"(i4)") xbnd(nx)
write(25,'(a)') '"band'//trim(adjustl(ch))//'.dat" u 1:2:($5-fermi) with pm3d title "spin down"'

end if

write(25,'(a)')
write(25,'(a)')
close(25)
end subroutine

end module
