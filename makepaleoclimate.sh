#!/usr/bin/bash

module load netCDF netCDF-Fortran

for namelist in `ls namelist/midHolocene*.namelist`
do

  tmp=${namelist##*/}
  
  outfile=${tmp%%.*}.nc
  
  echo "working on $outfile"

  ncgen -4 -o $outfile climate.cdl
  
  ./makepaleoclimate $namelist $outfile

done
