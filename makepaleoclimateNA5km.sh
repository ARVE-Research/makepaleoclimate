#!/usr/bin/env bash

module load netCDF-Fortran

cdl=/home/terraces/datasets/climate/NorthAm_hybrid/update2026/NAclimate5km.cdl

cat << END > modellist.txt
MRI-CGCM3
MPI-ESM-P
MIROC-ESM
IPSL-CM5A-LR
HadGEM2-ES
GISS-E2-R
CNRM-CM5
MMM
END

while read model
do

  namelist=midHolocene_$model.namelist

  outfile=paleoclimate/NA5km/${namelist%%.*}.nc
  
  echo "working on $outfile"

  ncgen -4 -o $outfile $cdl
  
  ./makepaleoclimate namelist/NA5km/$namelist $outfile
  
  ./makepaleolightning namelist_lightning/$namelist $outfile

done < modellist.txt
