!######################################################################
! Module : utilities
! Version: v0.0
! Author : Q. X. & J. X. Li
! Date   : 2017-08-21
! Content: Some useful functions
!######################################################################
module utilities
use const, only : pi
implicit none

contains

!
!     %-------------------%
!          date_time
!     %-------------------%
!
subroutine date_time()
integer :: t(8)
call date_and_time(values = t)
!write(6,"(a)") "+--------------------------------------------+"
write(6, 1000) t(1), t(2), t(3), t(5), t(6), t(7), t(8)
!write(6,"(a)") "+--------------------------------------------+"
1000  format('Date and time : ', i4.4, '-', i2.2, '-', i2.2, 2x, &
&       i2.2, ":", i2.2, ":", i2.2, ":", i3.3)
write(*,*)
end subroutine
!
!     %-------------------%
!          sayHello
!     %-------------------%
!

subroutine sayHi
write(6,"(a)") "######################################################################"
write(6,"(a)") "                     Welcome to ppvasp 0.0.1"
write(6,"(a)") "                     Q. X. & J. X. Li 2021-11-25"
write(6,"(a)") "                     /home/jxli/install/ppvasp4/v3"
write(6,"(a)") "######################################################################"
end subroutine

!
!     %--------------------%
!           write_time
!     %--------------------%
!
subroutine write_time( t )
real*8, intent(in) :: t
integer :: hour, minute, second

hour   = floor(t) / 3600
minute = floor(t - hour * 3600) / 60
second = floor(t - hour * 3600 - minute * 60)

write(6,"(a)")
if ( hour == 0 .and. minute == 0 ) then
write(6,"(a,i2,a)") &
& " Total time: ", second, " sec"
else if ( hour == 0 .and. minute /= 0 ) then
write(6,"(a,i2,a,i2,a)") &
& " Total time: ", minute, " min ", second, " sec"
else
write(6,"(a,i4,a,i2,a,i2,a)") &
& " Total time: ", hour, " h ", minute, " min ", second, " sec"
end if
end subroutine
!
!     %------------------%
!        check existence
!     %------------------%
!
subroutine is_file_exist(fn)
character(*), intent(in) :: fn
logical :: alive
inquire(file=fn,exist=alive)
if (.not.alive) then
write(*,"(a)") "Error: file "//trim(fn)//" doesn't exist"
stop
end if
end subroutine

!
!     %-------------------%
!          num of str
!     %-------------------%
!
integer function numofstr( str )
character(*), intent(in) :: str
character(len=512) :: s
integer :: i, l, n

s = adjustl(str)
n = len_trim(s)
l = 0
do i = 1, n
if (s(i:i) == " " .and. s(i+1:i+1) /= " ") l = l + 1
if (s(i:i) /= " " .and. s(i+1:i+1) == " ") l = l + 1
enddo
numofstr = l / 2 + 1
end function

!
!    %-----------------------------%
!        read_nline of a file
!    %-----------------------------%
!
integer function read_nline(fn) result(ans)
character(*), intent(in) :: fn
integer :: i, info
logical :: alive

inquire(file=fn,exist=alive)
if (.not.alive) then
write(6,"(a)") "Warning: "//trim(fn)//" doesn't exist in read_nline() !!!"
ans = 0
return
end if

open(unit=11,file=fn,status="old")
i = 0
info = 0
do while (.true.)
read(unit=11,fmt=*,iostat=info) ! something here?
if (info /= 0) exit
i = i + 1
enddo
close(11)
ans = i
end function

!
!       %-------------------%
!          Gaussian delta
!       %-------------------%
!
real*8 function Gaussian_delta(t,eta)
real*8, intent(in) :: t, eta
Gaussian_delta = exp(-(t/eta)**2/2.0)/(sqrt(2.0*pi)*eta)
end function
!
!       %-------------------%
!          Lorentz delta
!       %-------------------%
!
real*8 function Lorentz_delta(t,eta)
real*8, intent(in) :: t, eta
Lorentz_delta = eta/(t*t+eta*eta)/pi
end function


!
!      %--------------------%
!         get fermi energy
!      %--------------------%
!
subroutine get_fermi_energy(fermi)
real*8, intent(out) :: fermi
character(len=79) :: str
integer :: info = 0
logical :: alive

fermi = 0.0
inquire(file='fermi.dat', exist=alive)
write(6,'(a)') "Inquire file fermi.dat ..."
if(alive)then
open(unit=10,file='fermi.dat',status='old')
read(unit=10,fmt=*,iostat=info) fermi
if (info /= 0) then
read(10,'(a79)') str
read(str(11:24),*) fermi
end if
close(10)
write(6,'(a)') "Read E-fermi from fermi.dat."
else
write(6,'(a)') "fermi.dat doesn't exist."
endif
write(6,'(a,f8.4)') "E-fermi = ", fermi
end subroutine

end module
