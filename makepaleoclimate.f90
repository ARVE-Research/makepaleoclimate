program makepaleoclimate

! gfortran -o makepaleoclimate addvarmod.f90 makepaleoclimate.f90 -I/home/public/easybuild/software/netCDF-Fortran/4.6.1-gompi-2023a/include -lnetcdff

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
character(200) :: basefile
character(200) :: anompath
character(200) :: tminfile 
character(200) :: tmaxfile 
character(200) :: prefile 
character(200) :: wetfile 
character(200) :: cldfile 
character(200) :: windfile
character(200) :: lghtfile
character(200) :: outfile

character(20)  :: xvarname
character(20)  :: yvarname

integer :: status
integer :: bfid
integer :: afid
integer :: ofid
integer :: dimid
integer :: varid
integer :: xlen
integer :: ylen
integer :: tlen

integer :: var
integer :: x,y

integer(i2), parameter :: imissing = -32768

character(4) :: varname

logical :: projgrid = .false.
logical :: anom20cr = .false.

real(dp),    allocatable, dimension(:)     :: xcoord
real(dp),    allocatable, dimension(:)     :: ycoord
real(dp),    allocatable, dimension(:)     :: tcoord
real(dp),    allocatable, dimension(:,:)   :: tbounds
real(dp),    allocatable, dimension(:,:)   :: geocoord

real(sp),    allocatable, dimension(:,:,:) :: base
real(sp),    allocatable, dimension(:,:,:) :: anom
real(sp),    allocatable, dimension(:,:,:) :: clim

integer(i2), allocatable, dimension(:,:,:) :: var_in
integer(i2), allocatable, dimension(:,:,:) :: var_out

real(sp) :: scale_factor
real(sp) :: add_offset

real(sp), dimension(2) :: actual_range

character(4), dimension(7), parameter :: varnames = ['tmin','tmax','pre ','wet ','cld ','wnd ','lght']

integer, parameter, dimension(12) :: ndaymonth = [ 31,28,31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]  ! number of days in each month

namelist / joboptions / projgrid,basefile,anompath,tminfile,tmaxfile,prefile,wetfile,cldfile,windfile,lghtfile,anom20cr

! -------------------------------------------------------

call getarg(1,jobfile)
call getarg(2,outfile)

open(10,file=jobfile)
read(10,nml=joboptions)
close(10)

! ---------

if (projgrid) then
  xvarname = 'x'
  yvarname = 'y'
else
  xvarname = xvarname
  yvarname = yvarname
end if

status = nf90_open(basefile,nf90_nowrite,bfid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(bfid,xvarname,dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(bfid,dimid,len=xlen)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(bfid,yvarname,dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(bfid,dimid,len=ylen)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_dimid(bfid,'time',dimid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inquire_dimension(bfid,dimid,len=tlen)
if (status /= nf90_noerr) call handle_err(status)

allocate(xcoord(xlen))
allocate(ycoord(ylen))
allocate(tcoord(tlen))
allocate(tbounds(2,tlen))

allocate(base(xlen,ylen,tlen))
allocate(anom(xlen,ylen,tlen))
allocate(clim(xlen,ylen,tlen))

allocate(var_in(xlen,ylen,tlen))
allocate(var_out(xlen,ylen,tlen))

var_in  = imissing
var_out = imissing

! ---------

status = nf90_open(outfile,nf90_write,ofid)
if (status /= nf90_noerr) call handle_err(status)

! ----
! coordinate variables

! lon or easting

status = nf90_inq_varid(bfid,xvarname,varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(bfid,varid,xcoord)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_varid(ofid,xvarname,varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ofid,varid,xcoord)
if (status /= nf90_noerr) call handle_err(status)

! lat or northing

status = nf90_inq_varid(bfid,yvarname,varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(bfid,varid,ycoord)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_varid(ofid,yvarname,varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ofid,varid,ycoord)
if (status /= nf90_noerr) call handle_err(status)

! time

status = nf90_inq_varid(bfid,'time',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(bfid,varid,tcoord)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_varid(ofid,'time',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ofid,varid,tcoord)
if (status /= nf90_noerr) call handle_err(status)

! time bounds

status = nf90_inq_varid(bfid,'climatology_bounds',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(bfid,varid,tbounds)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_varid(ofid,'climatology_bounds',varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ofid,varid,tbounds)
if (status /= nf90_noerr) call handle_err(status)

! ----

if (projgrid) then

  allocate(geocoord(xlen,ylen))

  status = nf90_inq_varid(bfid,'lon',varid)
  if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_get_var(bfid,varid,geocoord)
  if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ofid,'lon',varid)
  if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_put_var(ofid,varid,geocoord)
  if (status /= nf90_noerr) call handle_err(status)

  status = nf90_inq_varid(bfid,'lat',varid)
  if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_get_var(bfid,varid,geocoord)
  if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ofid,'lat',varid)
  if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_put_var(ofid,varid,geocoord)
  if (status /= nf90_noerr) call handle_err(status)

  deallocate(geocoord)

end if

! ----
! elevation - could do with an anomaly, not yet implemented

varname = 'elv'

status = nf90_inq_varid(bfid,varname,varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(bfid,varid,var_in,start=[1,1,1],count=[xlen,ylen,1])
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(bfid,varid,'scale_factor',scale_factor)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(bfid,varid,'add_offset',add_offset)
if (status /= nf90_noerr) call handle_err(status)

var_out = var_in

actual_range(1) = real(minval(var_out,mask=var_out /= imissing))
actual_range(2) = real(maxval(var_out,mask=var_out /= imissing))

! --

status = nf90_inq_varid(ofid,varname,varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ofid,varid,var_out,start=[1,1,1],count=[xlen,ylen,1])
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_att(ofid,varid,'scale_factor',scale_factor)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_att(ofid,varid,'add_offset',add_offset)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_att(ofid,varid,'actual_range',actual_range)
if (status /= nf90_noerr) call handle_err(status)

! ----
! NB in this version we are not calculating lightning because we have gone over to the z-score anomaly method
! of calculating lightning. Use the program makepaleolightning.f90 after running this code.

if (anom20cr) then

  ! tmin, tmax, pre, wet, cld, wnd, lght
  
  call addvar(bfid,ofid,xlen,ylen,tlen,'tmin','tmin',trim(anompath)//tminfile,'abs')
  
  call addvar(bfid,ofid,xlen,ylen,tlen,'tmax','tmax',trim(anompath)//tmaxfile,'abs')
  
  call addvar(bfid,ofid,xlen,ylen,tlen,'pre','apcp',trim(anompath)//prefile,'abs',bounds=[0.,9999.])

  call addvar(bfid,ofid,xlen,ylen,tlen,'wet','wetf',trim(anompath)//wetfile,'abs',bounds=[0.,1.])
  
  call addvar(bfid,ofid,xlen,ylen,tlen,'cld','tcdc',trim(anompath)//cldfile,'abs',bounds=[0.,1.])
  
  call addvar(bfid,ofid,xlen,ylen,tlen,'wnd','wspd',trim(anompath)//windfile,'abs',bounds=[0.,9999.])
  
  ! call addvar(bfid,ofid,xlen,ylen,tlen,'lght','density',trim(anompath)//lghtfile,'abs',bounds=[0.,9999.])
  
else

  ! tmin, tmax, pre, cld, wnd, lght
  
  call addvar(bfid,ofid,xlen,ylen,tlen,'tmin','tasmin',trim(anompath)//tminfile,'abs')
  
  call addvar(bfid,ofid,xlen,ylen,tlen,'tmax','tasmax',trim(anompath)//tmaxfile,'abs')
  
  call addvar(bfid,ofid,xlen,ylen,tlen,'pre','pr',trim(anompath)//prefile,'abs',bounds=[0.,9999.])
  
  call addvar(bfid,ofid,xlen,ylen,tlen,'cld','clt',trim(anompath)//cldfile,'abs',bounds=[0.,1.])
  
  call addvar(bfid,ofid,xlen,ylen,tlen,'wnd','sfcWind',trim(anompath)//windfile,'abs',bounds=[0.,9999.])
  
  ! call addvar(bfid,ofid,xlen,ylen,tlen,'lght','prc',trim(anompath)//lghtfile,'rel',bounds=[0.,9999.])
  
  ! ----
  ! wet days
  ! need to precalculate a wetf anomaly by using statistical relationships to estimate wetf in GCM output
  ! see /home/terraces/datasets/CMIP5/midHolocene/precip-wetdays/ for scripts
  
  call addvar(bfid,ofid,xlen,ylen,tlen,'wet','wetf',trim(anompath)//wetfile,'abs',bounds=[0.,1.])

end if

! ----

status = nf90_close(bfid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_close(ofid)
if (status /= nf90_noerr) call handle_err(status)

! ---------

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

end program makepaleoclimate