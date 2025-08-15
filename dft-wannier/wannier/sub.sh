#!/bin/bash
source ~/.bashrc

#VASP="mpirun -np 24 /lfs/soft/vasp/vasp.6.3.2/bin/vasp_std"
export PATH=/lfs/jxli/utilities/AutoVASP3.0/bin:$PATH
export PATH=/lfs/soft/vasp/5.4.4/wannier90-3.1.0:$PATH
export PATH=/lfs/soft/vasp/5.4.4/vasp.5.4.4/bin:$PATH
#export PATH=/home/jxli/install/vasp/vasp5.3.4-ning:$PATH
VASPncl="mpirun -np 96 vasp_ncl"
VASP="mpirun -np 96 vasp_std"


echo -n "start time" >time
date >>time

#vasp cal for Wannier Projection
$VASP
mpirun -np 96 wannier90.x wannier90

