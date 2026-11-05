#!/usr/bin/env bash

module load netCDF-Fortran

cdl=/home/terraces/datasets/climate/NorthAm_hybrid/update2026/NAclimate5km.cdl

cat << END > modellist.txt
AWI-ESM-1-1-LR
INM-CM4-8
MIROC-ES2L
MPI-ESM1-2-LR
END

while read model
do

  namelist=LGM_$model.namelist

  outfile=paleoclimate/NA5km/${namelist%%.*}.nc
  
  echo "working on $outfile"

  ncgen -4 -o $outfile $cdl
  
  ./makepaleoclimate namelist/NA5km/$namelist $outfile
  
  ./makepaleolightning namelist_lightning/$namelist $outfile

done < modellist.txt
