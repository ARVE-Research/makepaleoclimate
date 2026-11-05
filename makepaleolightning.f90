program makepaleolightning

! gfortran -Wall -o makepaleolightning makepaleolightning.f90 -I/home/public/easybuild/software/netCDF-Fortran/4.6.1-gompi-2023a/include -lnetcdff

! generate a climatological mean paleoclimate by applying paleoclimate anomalies from a GCM to a standard baseline climate

use iso_fortran_env
use netcdf
use addvarmod, only : addvar

implicit none

integer, parameter :: i2 = int16
integer, parameter :: i4 = int32
integer, parameter :: i8 = int64
integer, parameter :: sp = real32
integer, parameter :: dp = real64

character(200) :: jobfile
character(200) :: climmeanfile
character(200) :: climstdfile
character(200) :: zscorefile 
character(200) :: outfile

! character(20)  :: xvarname
! character(20)  :: yvarname

integer :: status
integer :: ncid
integer :: dimid
integer :: varid

integer :: xlen
integer :: ylen
integer :: tlen

! integer :: var
! integer :: x,y
integer :: t

integer(i2), parameter :: imissing = -32768

! character(4) :: varname

! logical :: projgrid = .false.
! logical :: anom20cr = .false.

real(sp),    allocatable, dimension(:,:)   :: calcvar
real(sp),    allocatable, dimension(:,:,:) :: climmean
real(sp),    allocatable, dimension(:,:,:) :: climstd
real(sp),    allocatable, dimension(:,:,:) :: zscore

integer(i2), allocatable, dimension(:,:,:) :: var_out

real(sp) :: scale_factor
real(sp) :: add_offset

real(sp), dimension(2) :: actual_range

namelist / joboptions / climmeanfile,climstdfile,zscorefile

! -------------------------------------------------------

call getarg(1,jobfile)
call getarg(2,outfile)

open(10,file=jobfile)
read(10,nml=joboptions)
close(10)

! ---------
! open and read climatological mean

status = nf90_open(climmeanfile,nf90_nowrite,ncid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(ncid,'x',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(ncid,dimid,len=xlen)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(ncid,'y',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(ncid,dimid,len=ylen)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(ncid,'time',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(ncid,dimid,len=tlen)
if (status /= nf90_noerr) call handle_err(status)

write(0,*)'mean:',xlen,ylen,tlen

allocate(climmean(xlen,ylen,tlen))

status = nf90_inq_varid(ncid,'lght',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(ncid,varid,climmean)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_close(ncid)
if (status /= nf90_noerr) call handle_err(status)

! ---------
! open and read climatological standard deviation

status = nf90_open(climstdfile,nf90_nowrite,ncid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(ncid,'x',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(ncid,dimid,len=xlen)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(ncid,'y',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(ncid,dimid,len=ylen)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(ncid,'time',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(ncid,dimid,len=tlen)
if (status /= nf90_noerr) call handle_err(status)

write(0,*)'stdev:',xlen,ylen,tlen

allocate(climstd(xlen,ylen,tlen))

status = nf90_inq_varid(ncid,'lght',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(ncid,varid,climstd)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_close(ncid)
if (status /= nf90_noerr) call handle_err(status)

! ---------
! open and read standardized convective precipitation anomaly (z-scores)

status = nf90_open(zscorefile,nf90_nowrite,ncid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(ncid,'x',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(ncid,dimid,len=xlen)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(ncid,'y',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(ncid,dimid,len=ylen)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(ncid,'time',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(ncid,dimid,len=tlen)
if (status /= nf90_noerr) call handle_err(status)

write(0,*)'zscore:',xlen,ylen,tlen

allocate(zscore(xlen,ylen,tlen))

status = nf90_inq_varid(ncid,'acpcp',varid)
if (status == nf90_enotvar) status = nf90_inq_varid(ncid,'prc',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(ncid,varid,zscore)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_close(ncid)
if (status /= nf90_noerr) call handle_err(status)

! ---------
! open and write output

status = nf90_open(outfile,nf90_write,ncid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_varid(ncid,'lght',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(ncid,varid,'scale_factor',scale_factor)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(ncid,varid,'add_offset',add_offset)
if (status /= nf90_noerr) call handle_err(status)

write(0,*)'calculating',scale_factor,add_offset

! calculate lightning as mean + stdev * zscore

allocate(calcvar(xlen,ylen))
allocate(var_out(xlen,ylen,tlen))

do t = 1,tlen

  ! sometimes the convective precip z-score anomaly gets very large especially if the climatological standard deviation is very small
  ! testing limiting the range of the z-score to +/-5 SD when estimating lightning.

  zscore(:,:,t) = min(max(zscore(:,:,t),-5.),5.)  

  calcvar = climmean(:,:,t) + climstd(:,:,t) * zscore(:,:,t)

  ! write(0,*)t,minval(calcvar),maxval(calcvar)

  calcvar = min(max(calcvar,0.),0.65534)

  var_out(:,:,t) = nint((calcvar - add_offset) / scale_factor,i2)
  
  ! write(0,*)t,minval(var_out(:,:,t)),maxval(var_out(:,:,t))

end do

status = nf90_put_var(ncid,varid,var_out)
if (status /= nf90_noerr) call handle_err(status)

actual_range = [minval(real(var_out) * scale_factor + add_offset),maxval(real(var_out) * scale_factor + add_offset)]
  
write(0,*)actual_range

status = nf90_put_att(ncid,varid,'actual_range',actual_range)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_close(ncid)
if (status /= nf90_noerr) call handle_err(status)

contains

! ---------------------

subroutine handle_err(status)

! Internal subroutine - checks error status after each netcdf call,
! prints out text message each time an error code is returned. 

integer, intent (in) :: status

if(status /= nf90_noerr) then 
  write(0,*)'NetCDF error: ',trim(nf90_strerror(status))
  stop
end if

end subroutine handle_err

! ---------------------

end program makepaleolightning
