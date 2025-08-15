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
integer :: nkpt, nbnd, nion, nobt

character(len=79) :: s
character(len=4) :: ch1, ch2
character(len=3), allocatable :: obts(:)

real*8, allocatable :: x(:), e(:,:), proj(:,:,:,:)


integer :: lidx1, lidx2, lidx3, lidx4
integer :: nskip ! skip

character(len=40) :: filename
real*8 :: tmp
integer :: icount, i, j, k
integer :: iobt, iion, ion, ipro, is, ikpt, ibnd
integer :: info = 0
logical alive


!
! START
!
inquire(file="PROCAR",exist=alive)
if (.not. alive) then
write(6,"(a)") "Warning: PROCAR file doesn't exist!!!"
return
end if


inquire(file="bands.dat",exist=alive)
if (.not. alive) then
write(6,"(a)") "Warning: bands.dat doesn't exist!!!"
write(6,"(a)") "please calculate bands.dat first!!!"
return
end if


! the position of first 'band'
open(unit=11,file='PROCAR',status='old')


icount = 0
do while (.true.)
read(unit=11,fmt="(a4)",iostat=info) s
icount = icount + 1
if (s == 'band' .or. info /= 0) then
lidx1 = icount
exit
end if
enddo


! the position of 'ion'
do while (.true.)
read(unit=11,fmt="(a3)",iostat=info) s
icount = icount + 1
if (s == 'ion' .or. info /= 0) then
lidx2 = icount
exit
end if
enddo


! the position of 'tot'
do while (.true.)
read(unit=11,fmt="(a3)",iostat=info) s
icount = icount + 1
if (s == 'tot' .or. info /= 0) then
lidx3 = icount
exit
end if
enddo

! the second 'band'
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
read(s(40:43),*) nbnd
read(s(63:66),*) nion


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
read(s(4:73),*) (obts(i), i=1,nobt)


do i = 1, 6
backspace(11)
enddo

allocate(proj(nobt,nion,nbnd,nkpt))

kpointloop: do k = 1, nkpt
read(11,*)
read(11,*) s, ikpt
read(11,*)

if( s /= 'k-point' .or. ikpt/=k) then
write(6,"(a)") "Error: ikpt mismatch when reading PROCAR!!!"
stop
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
read(11,*)  ion, (proj(ipro,i,j,k) , ipro = 1,nobt )  ! ignore tot
if(i /= ion) then
write(6,"(a)") "Error: ion mismatch when reading PROCAR!!!"
stop
end if
enddo ionloop

do is = 1, nskip
read(11,*)
enddo

enddo bandloop
enddo kpointloop
close(11)


! read bands.dat
open(unit=12,file='bands.dat',status='old')
allocate(x(nkpt), e(nkpt,nbnd))
do i = 1, nkpt
read(12,*) x(i)
enddo

rewind(12)
do j = 1,nbnd
do k = 1,nkpt
read(12,*) tmp, e(k, j)
enddo
read(12,*)
enddo
close(12)


! 1st layer
do iion = 1, nion
write(ch1,"(i4)") iion
filename = "projband"//trim(adjustl(ch1))//'.dat'
write(*,"(a)") "Write projband to "//trim(adjustl(filename))
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
write(unit=21,fmt='(2f12.6)',advance='no')  x(i), e(i,j)
! 4th layer
do iobt = 1, nobt
!
! Note proj(:,1,:,:) is the total projected band
! 2,3,4 are the sx, sy, sz components
!
write(unit=21,fmt='(f7.3)',advance='no') proj(iobt,iion,j,i)
enddo
write(21,*)
enddo
write(21,*)
enddo
close(21)
enddo

call write_gnu_proj_band(fermi,nbnd,nion,nobt,obts)

deallocate(x,e,proj,obts)
end subroutine


!
!    %-------------------------------%
!          write_gnu_proj_band
!    %-------------------------------%
!
subroutine write_gnu_proj_band(fermi,nbnd,nion,nobt,obts)
real*8, intent(in) :: fermi
integer, intent(in) :: nbnd
integer, intent(in) :: nion
integer, intent(in) :: nobt
character(len=3), intent(in) :: obts(nobt)

character(len=4) :: ch, ch2
integer :: i, j


do i = 1, nion
write(ch,"(i4)") i

write(*,"(a)") "Write a gnuplot script example ---> plotprojband"//trim(adjustl(ch))//".gnu"
open(unit=25,file="plotprojband"//trim(adjustl(ch))//".gnu",status="unknown")
write(25,"(a)") '###################################################'
write(25,"(a)") '#### please copy the head of plotband.gnu here ####'
write(25,"(a)") '###################################################'
write(25,"(a)") 'load "projhead.gnu"'
write(25,"(a)")
write(25,"(a)") 'set key box opaque'
write(25,"(a)")
write(25,"(a)") 'sf = 10'
write(25,"(a)")
write(25,"(a)") "#choose your atom ->"
write(25,"(a)") "file = 'projband"//trim(adjustl(ch))//".dat'"
write(25,"(a)")
write(25,"(a,f8.4)") "fermi = ", fermi
write(25,"(a)")
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
write(25,"(a)")
write(25,"(a)")
close(25)
enddo
end subroutine

end module
