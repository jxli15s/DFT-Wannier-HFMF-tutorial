!######################################################################
! Module : procar
! Version: v0.0
! Author : Q. X. & J. X. Li
! Date   : 2018-12-15
! Content: PROCAR related
!######################################################################
module procar
use utilities, only : numofstr 
implicit none

contains

subroutine read_procar(fermi)
real*8, intent(in) :: fermi
integer :: nkpt, nbnd, nion, nobt, nspin

character(len=256) :: s
character(len=4) :: ch1, ch2
character(len=2) :: spname(2)
character(len=3), allocatable :: obts(:)

real*8, allocatable :: x(:)

!nspin,nkpt,nbnd
real*8, allocatable :: e(:,:,:)

!nobt,nion,nbnd,nkpt,nspin
real*8, allocatable :: proj(:,:,:,:,:)

!nobt,nbnd,nkpt,nspin
real*8, allocatable :: proj_tot(:,:,:,:)

integer :: lidx1, lidx2, lidx3, lidx4
integer :: nskip ! skip

character(len=40) :: filename
real*8 :: tmp
integer :: icount, i, j, k
integer :: iobt, iion, ion, ipro, is, ikpt, ibnd, iskp
integer :: info = 0
logical alive


!
! START
!
inquire(file="PROCAR",exist=alive)
if (.not. alive) then
write(6,"(a)") "Warning: PROCAR file doesn't exist!!!"
stop
end if


inquire(file="bands.dat",exist=alive)
if (.not. alive) then
write(6,"(a)") "Warning: bands.dat doesn't exist!!!"
write(6,"(a)") "please calculate bands.dat first!!!"
stop
end if

! read bands.dat
open(unit=12,file='bands.dat',status='old')
read(12,"(a79)") s
nspin = numofstr(s)
nspin = nspin - 1
backspace(12)



open(unit=11,file='PROCAR',status='old')

! the position of first 'band'   lidx1
icount = 0
do while (.true.)
read(unit=11,fmt="(a4)",iostat=info) s
icount = icount + 1
if (s == 'band' .or. info /= 0) then
lidx1 = icount
exit
end if
enddo


! the position of 'ion'   lidx2
do while (.true.)
read(unit=11,fmt="(a3)",iostat=info) s
icount = icount + 1
if (s == 'ion' .or. info /= 0) then
lidx2 = icount
exit
end if
enddo


! the position of 'tot'  lidx3
do while (.true.)
read(unit=11,fmt="(a3)",iostat=info) s
icount = icount + 1
if (s == 'tot' .or. info /= 0) then
lidx3 = icount
exit
end if
enddo

! the second 'band'  lidx4
do while (.true.)
read(unit=11,fmt="(a4)",iostat=info) s
icount = icount + 1
if (s == 'band' .or. info /= 0) then
lidx4 = icount
exit
end if
enddo

nskip = lidx4 - lidx3

!
! Read PROCAR from the beginning
!
rewind(11)
read(11,*)
read(11,'(a79)') s

read(s(15:20),*) nkpt
read(s(40:45),*) nbnd
read(s(64:68),*) nion

write(6,'(a,i6)') "spin = ", nspin
write(6,'(a,i6)') "nkpt = ", nkpt
write(6,'(a,i6)') "nbnd = ", nbnd
write(6,'(a,i6)') "nion = ", nion


do i = 1, 5
read(11,*)
enddo

read(11,'(a256)') s
nobt = numofstr(s)
nobt = nobt - 1  ! include tot column
allocate(obts(nobt))
read(s(4:256),*) (obts(i), i=1,nobt)


do i = 1,7
backspace(11)
enddo


allocate(proj(nobt,nion,nbnd,nkpt,nspin))
allocate(proj_tot(nobt,nbnd,nkpt,nspin))

spinloop: do is = 1, nspin
read(11,*) ! # of k-points

kpointloop: do k = 1, nkpt
read(11,*)
read(11,*) s, ikpt
read(11,*)

if( s /= 'k-point' .or. ikpt/=k) then
! for HSE06 
write(6,"(a)") "Warning: ikpt mismatch when reading PROCAR!!!"

!write(6,"(a)") "Error: ikpt mismatch when reading PROCAR!!!"
!stop
endif

bandloop: do j = 1, nbnd

read(11,*) s, ibnd
read(11,*)
read(11,*)  ! ion s py pz ...

if (s /= 'band' .or. ibnd /= j) then
write(6,"(a)") "Error: ibnd mismatch when reading PROCAR!!!"
stop
endif

ionloop: do i = 1, nion
read(11,*)  ion, (proj(ipro,i,j,k,is) , ipro = 1,nobt )  ! ignore ion_num
if(i /= ion) then
write(6,"(a)") "Error: ion mismatch when reading PROCAR!!!"
stop
end if
enddo ionloop

read(11,*) s, (proj_tot(ipro,j,k,is), ipro = 1,nobt) !ignore tot
do iskp = 1, nskip-1
read(11,*)
enddo

enddo bandloop
enddo kpointloop
enddo spinloop

close(11)


! read bands.dat
allocate(x(nkpt))
do i = 1, nkpt
read(12,*) x(i)
enddo

rewind(12)
allocate(e(nspin,nkpt,nbnd))
do j = 1,nbnd
do k = 1,nkpt
read(12,*) tmp, e(1:nspin,k, j)
enddo
read(12,*)
enddo
close(12)


if (nspin == 2) then
spname(1) = "up"
spname(2) = "dn"
end if

do is =1, nspin
! 1st layer
if (nspin == 2) then
filename = "proj"//spname(is)//'.dat'
else
filename = "proj.dat"
end if

write(*,"(a)") "Write band to "//trim(adjustl(filename))
open(unit=21,file=trim(filename),status='unknown')

write(unit=21,fmt="(a)",advance="no") "#  k-distance    energy     "
do iobt = 1, nobt
write(unit=21,fmt="(a,a)",advance="no") obts(iobt), "    "
enddo
write(21,*)

!2nd layer
do j = 1, nbnd
!3rd layer
do i = 1, nkpt
write(unit=21,fmt='(2f12.6)',advance='no') x(i), e(is,i,j)
!4th layer
do iobt = 1, nobt
write(unit=21,fmt='(f7.3)',advance="no") proj_tot(iobt,j,i,is)
enddo
write(21,*)
enddo
write(21,*)
enddo
close(21)
enddo

do is = 1, nspin
! 1st layer
do iion = 1, nion
write(ch1,"(i4)") iion

if (nspin == 2) then
filename = "projatom"//spname(is)//trim(adjustl(ch1))//'.dat'
else
filename = "projatom"//trim(adjustl(ch1))//'.dat'
end if

write(*,"(a)") "Write band to "//trim(adjustl(filename))
open(unit=21,file=trim(filename),status='unknown')

write(unit=21,fmt="(a)",advance="no") "#  k-distance    energy     "
do iobt = 1, nobt
write(unit=21,fmt="(a,a)",advance="no") obts(iobt), "    "
enddo
write(21,*)

! 2nd layer
do j = 1, nbnd
! 3rd layer
do i = 1, nkpt
write(unit=21,fmt='(2f12.6)',advance='no')  x(i), e(is,i,j)
! 4th layer
do iobt = 1, nobt
!
! Note proj(:,1,:,:) is the total projected band
! 2,3,4 are the sx, sy, sz components
!
write(unit=21,fmt='(f7.3)',advance='no') proj(iobt,iion,j,i,is)
enddo
write(21,*)
enddo
write(21,*)
enddo
close(21)
enddo
enddo


call write_gnu_proj_band(fermi,nspin,nbnd,nion,nobt,obts)

deallocate(x,e,proj,obts,proj_tot)
end subroutine


!
!    %-------------------------------%
!          write_gnu_proj_band
!    %-------------------------------%
!
subroutine write_gnu_proj_band(fermi,nspin,nbnd,nion,nobt,obts)
real*8, intent(in) :: fermi
integer, intent(in) :: nspin
integer, intent(in) :: nbnd
integer, intent(in) :: nion
integer, intent(in) :: nobt
character(len=3), intent(in) :: obts(nobt)

character(len=4) :: ch, ch2
character(len=79) :: fname, s, ss
character(len=2) :: spname(2)
logical :: alive

integer :: i, j, info

if (nspin == 2) then
spname(1) = "up"
spname(2) = "dn"
end if

inquire(file="plotband.gnu",exist=alive)
if (alive) then
open(unit=13,file="plotband.gnu",status="old")
open(unit=14,file="projhead.gnu",status="replace")
do while (.true.)
read(unit=13,fmt="(a79)",iostat=info) s
read(s(1:4),"(a)") ss
if (ss == 'plot' .or. info /= 0) exit
write(14,"(a)") trim(s)
enddo
close(13)
close(14)
end if

do i = 1, nion
write(ch,"(i4)") i

fname = "plotprojatom"//trim(adjustl(ch))//".gnu"
write(*,"(a)") &
& "Write a gnuplot script example ---> "//trim(adjustl(fname))
open(unit=25,file=trim(adjustl(fname)),status="unknown")
if (.not. alive) then
write(25,"(a)") "### projhead.gnu doesn't exist !!! "
write(25,"(a)") "### please modify plotband.gnu and create projhead.gnu file first"
end if
write(25,"(a)") 'load "projhead.gnu"'
write(25,"(a)") 'set title "Atom: '//trim(adjustl(ch))//'"'
write(25,"(a,f8.4)") "fermi = ", fermi
write(25,"(a)") 'set key box opaque'
write(25,"(a)") 'sf = 5'
write(25,"(a)") '#set yrange [ -2 : 2 ]'

if (nspin == 1) then
write(25,"(a)") "#choose your atom ->"
write(25,"(a)") "file = 'projatom"//trim(adjustl(ch))//".dat'"
write(25,"(a)") "plot  \"
write(25,"(a)") "$vl w l ls 47 dt (10,7) notitle ,\"
write(25,"(a)") "$fl w l ls 47  notitle ,\"
write(25,"(a)") '"bands.dat" u 1:($2-fermi) w l ls 47 notitle ,\'
do j = 1, nobt-2
write(ch2,"(i4)") j+2
write(25,"(a,i4,a)") 'file u ($1):($2-fermi):($'//trim(adjustl(ch2))//'*sf) w p ls ', 2+j, &
& '  pt 6 ps variable title "'//trim(adjustl(obts(j)))//'"  ,\'
enddo
write(ch2,"(i4)") nobt+1
write(25,"(a,i4,a)") 'file u ($1):($2-fermi):($'//trim(adjustl(ch2))//'*sf) w p ls ', 1+nobt, &
& '  pt 6 ps variable title "'//trim(adjustl(obts(nobt-1)))//'" '
! Note: do not plot tot part
write(25,"(a)")
write(25,"(a)")

else if (nspin == 2) then

write(25,"(a)") &
& "set multiplot layout 1,2 title 'projected bands for spin up and down' font 'Times-Roman,15'"
write(25,"(a)")
write(25,"(a)") "##############################"
write(25,"(a)") "# the spin up part"
write(25,"(a)") "##############################"
write(25,"(a)") "set origin 0, 0.25"
write(25,"(a)") "set size 0.5, 0.5"
write(25,"(a)") "set title 'spin up'"
write(25,"(a)") "#choose your atom ->"
write(25,"(a)") "file = 'projatomup"//trim(adjustl(ch))//".dat'"
write(25,"(a)") "plot  \"
write(25,"(a)") "$vl w l ls 47 dt (10,7) notitle ,\"
write(25,"(a)") "$fl w l ls 47  notitle ,\"
write(25,"(a)") '"bands.dat" u 1:($2-fermi) w l ls 47 notitle ,\'
do j = 1, nobt-2
write(ch2,"(i4)") j+2
write(25,"(a,i4,a)") 'file u ($1):($2-fermi):($'//trim(adjustl(ch2))//'*sf) w p ls ', 2+j, &
& '  pt 6 ps variable title "'//trim(adjustl(obts(j)))//'"  ,\'
enddo
write(ch2,"(i4)") nobt+1
write(25,"(a,i4,a)") 'file u ($1):($2-fermi):($'//trim(adjustl(ch2))//'*sf) w p ls ', 1+nobt, &
& '  pt 6 ps variable title "'//trim(adjustl(obts(nobt-1)))//'" '
! Note: do not plot tot part
write(25,"(a)")
write(25,"(a)") "##############################"
write(25,"(a)") "# the spin down part"
write(25,"(a)") "##############################"
write(25,"(a)") "set origin 0.5, 0.25"
write(25,"(a)") "set size 0.5, 0.5"
write(25,"(a)") "set title 'spin down'"
write(25,"(a)") "#choose your atom ->"
write(25,"(a)") "file = 'projatomdn"//trim(adjustl(ch))//".dat'"
write(25,"(a)") "plot  \"
write(25,"(a)") "$vl w l ls 47 dt (10,7) notitle ,\"
write(25,"(a)") "$fl w l ls 47  notitle ,\"
write(25,"(a)") '"bands.dat" u 1:($3-fermi) w l ls 47 notitle ,\'
do j = 1, nobt-2
write(ch2,"(i4)") j+2
write(25,"(a,i4,a)") 'file u ($1):($2-fermi):($'//trim(adjustl(ch2))//'*sf) w p ls ', 2+j, &
& '  pt 6 ps variable title "'//trim(adjustl(obts(j)))//'"  ,\'
enddo
write(ch2,"(i4)") nobt+1
write(25,"(a,i4,a)") 'file u ($1):($2-fermi):($'//trim(adjustl(ch2))//'*sf) w p ls ', 1+nobt, &
& '  pt 6 ps variable title "'//trim(adjustl(obts(nobt-1)))//'" '
! Note: do not plot tot part
write(25,"(a)")
write(25,"(a)") "unset multiplot"

end if

write(25,"(a)")
write(25,"(a)")
close(25)
enddo

end subroutine

end module
