!######################################################################
! Module : const
! Version: v0.0
! Author : Q. X. & J. X. Li
! Date   : 2017-08-21
! Content: Some constant
!######################################################################
module const

complex*16, parameter :: iz = (0.0, 1.0)

real*8, parameter :: pi = 3.1415926535897932384626433832795028

real*8, parameter :: eps = 1e-12

! 1.0 for vasp
real*8, parameter :: reciprocal_scale = 2.0*pi


! Pauli matrices
complex*16 :: sx(2,2), sy(2,2), sz(2,2), sp(2,2), sm(2,2)
data sx /(0.0,0.0), (1.0,0.0), &
&        (1.0,0.0), (0.0,0.0)/
data sy /(0.0,0.0), (0.0,1.0), &
&        (0.0,-1.0), (0.0,0.0)/
data sz /(1.0,0.0), (0.0,0.0), &
&        (0.0,0.0), (-1.0,0.0)/
data sp /(0.0,0.0), (0.0,0.0), &
&        (2.0,0.0), (0.0,0.0)/
data sm /(0.0,0.0), (2.0,0.0), &
&        (0.0,0.0), (0.0,0.0)/

end module
