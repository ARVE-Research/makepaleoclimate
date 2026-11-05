#!/usr/bin/env bash

# -----

cat << END > modellist.txt
AWI-ESM-1-1-LR
INM-CM4-8
MIROC-ES2L
MPI-ESM1-2-LR
END

while read model
do

  echo $model
  
  sed -e "s/MODEL/$model/g" namelist/NA5km/LGM_TEMPLATE.namelist > namelist/NA5km/LGM_$model.namelist
  
  sed -e "s/MODEL/$model/g" namelist_lightning/LGM_TEMPLATE.namelist > namelist_lightning/LGM_$model.namelist

done < modellist.txt

exit

# -----

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

  echo $model
  
  sed -e "s/MODEL/$model/g" namelist/NA5km/midHolocene_TEMPLATE.namelist > namelist/NA5km/midHolocene_$model.namelist
  
  sed -e "s/MODEL/$model/g" namelist_lightning/midHolocene_TEMPLATE.namelist > namelist_lightning/midHolocene_$model.namelist

done < modellist.txt
