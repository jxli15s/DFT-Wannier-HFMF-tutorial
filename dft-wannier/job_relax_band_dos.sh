#!/bin/bash
source ~/.bashrc

#export PATH=/lfs/soft/vasp/vasp.6.3.2/bin:$PATH
export PATH=/lfs/soft/vasp/5.4.4/vasp.5.4.4/bin:$PATH

VASP="mpirun -np 96 vasp_std"
VASPncl="mpirun -np 96 vasp_ncl"
home=`pwd`

clc="rm -r CHG* WAVECAR DOSCAR EIGENVAL ICONST PENALTYPOT REPORT HILLSPOT IBZKPT vasprun.xml"


#$clc

cat > INCAR <<!
ENCUT = 500 ;
ISMEAR = 0 ; SIGMA = 0.05
EDIFF= 1E-6 ; NELM = 200
EDIFFG= -1E-2 ; IBRION=2 ; POTIM=0.20;  NSW=100 ; ISIF=2
KPAR = 4 ; NCORE = 12
!
#echo -e "102\n2\n0.03" |vaspkit
$VASP
cp CONTCAR CONTCAR.1
cp OUTCAR OUTCAR.1

cp CONTCAR POSCAR
cat > INCAR <<!
ENCUT = 500
ISMEAR = 0 ; SIGMA = 0.05
EDIFF= 1E-6 ; NELM = 200
EDIFFG= -5E-3 ; IBRION=2 ; POTIM=0.20;  NSW=100 ; ISIF=2
KPAR = 4 ; NCORE = 12
!
#echo -e "102\n2\n0.03" |vaspkit
$VASP
cp CONTCAR CONTCAR.2
cp OUTCAR OUTCAR.2
#
cp CONTCAR POSCAR
cat > INCAR <<!
ENCUT = 500
ISMEAR = 0 ; SIGMA = 0.05
EDIFF= 1E-6 ; NELM = 200
EDIFFG= -1E-3 ; IBRION=1 ; POTIM=0.10;  NSW=100 ; ISIF=2
KPAR = 4 ; NCORE = 12
!
#echo -e "102\n2\n0.03" |vaspkit
$VASP
cp CONTCAR CONTCAR.1
cp OUTCAR OUTCAR.1
rm -rf WAVECAR
rm -rf CHGCAR
rm -rf CHG

#
cp CONTCAR POSCAR
cat > INCAR <<!
ENCUT = 500
ISMEAR = 0 ; SIGMA = 0.05
EDIFF= 1E-6 ; NELM = 200
IBRION = -1 ; NSW = 0
KPAR = 4 ; NCORE = 12
LORBIT = 11
NBANDS = 96
!
#echo -e "102\n2\n0.03" |vaspkit
$VASP
#cp OUTCAR OUTCAR.DFT


mkdir $home/band
cd $home/band
cp ../POSCAR ./
cp ../POTCAR ./
cp ../KPOINTS-band ./KPOINTS
#echo -e "303" |vaspkit ; cp KPATH.in KPOINTS
ln -s ../WAVECAR ./
ln -s ../CHGCAR ./
cat > INCAR <<!
ENCUT = 500
ISMEAR = 0 ; SIGMA = 0.05
EDIFF= 1E-6 ; NELM = 200
IBRION = 1 ; NSW = 0
KPAR = 4 ; NCORE = 12
LWAVE = .FALSE. ; LCHARG = .FALSE.
ISTART = 1 ; ICHARG = 11
LORBIT = 11
NBANDS = 96
!
$VASP

