# DFT-Wannier-HFMF-tutorial

This repository provides a complete workflow from **first-principles DFT calculations** to **Wannier projection** and **tight-binding Hartree–Fock mean-field (HFMF) calculations**, including **post-processing tools** for VASP output.  
It is intended for researchers working on electronic structure calculations, model construction, and correlated electron physics.

---

## Repository Structure

- **dft-wannier** : DFT → Wannier pipeline (Bash scripts + input files)  
- **ppvasp** : Post-processing tools for VASP (Fortran)  
- **TB-HFMF** : Tight-binding + HFMF package (MATLAB)  
- **README.md** : Documentation  

---

## 1. dft-wannier: DFT to Wannier90 Workflow

This folder contains **bash scripts** integrating **all steps** from structure relaxation to Wannier projection, using **VASP** as the DFT engine.

**Features:**
- Structure optimization (geometry relaxation)
- Self-consistent field (SCF) calculation
- Band structure calculation
- Wannier90 projection for graphene (customizable to other materials)
- All input files (`POSCAR`, `INCAR`, `KPOINTS`, `POTCAR`, `.win`, etc.) included
- Fully automated: a single bash script runs the complete pipeline

---

## 2. ppvasp: VASP Post-Processing Package

A **Fortran-based** post-processing package for VASP output, designed for band structure and density-of-states (DOS) visualization.

**Capabilities:**
- Standard band structure plotting
- Density of States (DOS) plotting
- 3D band dispersion visualization
- Fat bands plotting (orbital-projected bands)

---

## 3. TB-HFMF: Tight-Binding and HFMF MATLAB Package

A **MATLAB** toolbox for constructing and analyzing tight-binding models, with direct **Wannier90 interface**.

**Features:**
- Import tight-binding parameters from Wannier90 output
- Solve single-particle problems
- Perform Hartree–Fock mean-field (HFMF) calculations for electron–electron interactions
- Handle arbitrary lattice geometries and orbital degrees of freedom
- Useful for correlated insulator, superconductivity, and topological phase studies

---

## Dependencies

- **DFT stage**: [VASP](https://www.vasp.at/), [Wannier90](http://www.wannier.org/)  
- **Post-processing**: Fortran compiler (e.g., `gfortran`)  
- **Model calculations**: MATLAB (R2020a or later recommended), with basic linear algebra toolboxes  
- **OS**: Linux or macOS (tested on Ubuntu 20.04, macOS 13.x)  

---

## How to Use

**Step 1: DFT → Wannier**
```bash
cd dft-wannier
bash run_all.sh
```

**Step 2: Post-Processing**
```bash
cd ppvasp
make
./band.x
./dos.x
```

**Step 3: Tight-Binding + HFMF**
```matlab
cd TB-HFMF
run main_tb_hfmf.m
```

---

## Workflow Overview

1. **DFT Calculation** – Perform structure optimization, SCF, and band structure calculations in VASP.  
2. **Wannier Projection** – Use Wannier90 to obtain localized orbitals and tight-binding parameters.  
3. **Post-Processing** – Visualize band structures, DOS, and orbital characters using `ppvasp`.  
4. **Tight-Binding + HFMF** – Import Wannier TB models into MATLAB, solve single-particle problems or apply HFMF for interaction effects.  

---

## License
MIT License — see the LICENSE file for details.
