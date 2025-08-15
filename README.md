# DFT-Wannier-HFMF Tutorial

This repository bundles three related toolsets for electronic-structure studies:

- **dft-wannier/** – Bash scripts and input files for a graphene workflow using VASP and Wannier90. The `job_relax_band_dos.sh` script runs structural relaxation, self-consistent field (SCF) cycles, band calculations, and Wannier projection in sequence. Output is written to `dft-wannier/wannier/`.
- **ppvasp/** – A collection of Fortran utilities for post-processing VASP results. After compilation you can generate band structures, densities of states (DOS), 3D band surfaces, and fat-band plots.
- **TB-HFMF/** – MATLAB routines for building tight-binding models and solving Hartree–Fock mean-field (HFMF) problems. The `+MTB` module provides an interface to Wannier90 output.

## Quick Start

### Clone the repository
```bash
git clone git@github.com:jxli15s/DFT-Wannier-HFMF-tutorial.git
cd DFT-Wannier-HFMF-tutorial
```

### Run the DFT-Wannier workflow
```bash
cd dft-wannier
bash job_relax_band_dos.sh
```
The script performs relaxation, SCF, band-structure calculation, and Wannier90 projection consecutively.

### Compile the ppvasp utilities
```bash
cd ppvasp
make
```
After compilation the executables for band, DOS, 3D band, and fat-band analysis are available in the same directory.

### Use the TB-HFMF toolkit
The `TB-HFMF` directory contains MATLAB scripts for constructing tight-binding models, visualizing bands, and solving HFMF equations. It can import Wannier functions through the `+MTB` interface to Wannier90.

## License

