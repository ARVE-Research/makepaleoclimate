module addvarmod

use iso_fortran_env

implicit none

integer, parameter :: i2 = int16
integer, parameter :: i4 = int32
integer, parameter :: i8 = int64
integer, parameter :: sp = real32
integer, parameter :: dp = real64

contains

! ---------------------

subroutine addvar(bfid,ofid,xlen,ylen,tlen,bvarname,avarname,anomfile,anomtype,bounds)

use netcdf

implicit none

! arguments

integer,      intent(in) :: bfid
integer,      intent(in) :: ofid
integer,      intent(in) :: xlen
integer,      intent(in) :: ylen
integer,      intent(in) :: tlen
character(*), intent(in) :: bvarname
character(*), intent(in) :: avarname
character(*), intent(in) :: anomfile
character(*), intent(in) :: anomtype
real(sp), dimension(2), intent(in), optional :: bounds

! parameters

integer(i2), parameter :: imissing = -32768
integer(i2), parameter :: rmissing = -9999.

! number of days in each month

integer, parameter, dimension(12) :: ndaymonth = [31,28,31,30,31,30,31,31,30,31,30,31]  

! local variables

integer :: status
integer :: afid
integer :: varid
integer :: m

real(sp),    allocatable, dimension(:,:,:) :: base
real(sp),    allocatable, dimension(:,:,:) :: anom
real(sp),    allocatable, dimension(:,:,:) :: clim

integer(i2), allocatable, dimension(:,:,:) :: var_in
integer(i2), allocatable, dimension(:,:,:) :: var_out

real(sp) :: scale_factor
real(sp) :: add_offset

real(sp), dimension(2) :: actual_range

! ---

allocate(base(xlen,ylen,tlen))
allocate(anom(xlen,ylen,tlen))
allocate(clim(xlen,ylen,tlen))

allocate(var_in(xlen,ylen,tlen))
allocate(var_out(xlen,ylen,tlen))

var_in  = imissing
var_out = imissing

! ---
! reading

status = nf90_inq_varid(bfid,bvarname,varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(bfid,varid,var_in)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(bfid,varid,'scale_factor',scale_factor)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_att(bfid,varid,'add_offset',add_offset)
if (status /= nf90_noerr) call handle_err(status)

base = real(var_in) * scale_factor + add_offset

write(0,*)'base',minval(base,mask=var_in /= imissing),maxval(base,mask=var_in /= imissing)

! --

status = nf90_open(anomfile,nf90_nowrite,afid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_inq_varid(afid,avarname,varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_get_var(afid,varid,anom)
if (status /= nf90_noerr) call handle_err(status)

! ---
! calculating

clim = rmissing

if (trim(avarname) == 'pr') then
  do m = 1,tlen
    anom(:,:,m) = anom(:,:,m) * 86400. * real(ndaymonth(m))
  end do
end if

if (trim(avarname) == 'clt' .or. trim(avarname) == 'tcdc') anom = anom / 100.

if (trim(anomtype) == 'abs') then

  write(0,*)'add anomaly'

  clim = base + anom
  
else if (trim(anomtype) == 'rel') then

  write(0,*)'multiply anomaly'

  clim = base * anom

end if

if (present(bounds)) then
  clim = max(clim,bounds(1))
  clim = min(clim,bounds(2))
end if

where (var_in /= imissing) 
  var_out = nint((clim - add_offset) / scale_factor)
elsewhere
  var_out = imissing
end where

actual_range(1) = minval(real(var_out) * scale_factor + add_offset,mask=var_out /= imissing)
actual_range(2) = maxval(real(var_out) * scale_factor + add_offset,mask=var_out /= imissing)

! ---
! writing

status = nf90_inq_varid(ofid,bvarname,varid)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_var(ofid,varid,var_out)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_att(ofid,varid,'scale_factor',scale_factor)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_att(ofid,varid,'add_offset',add_offset)
if (status /= nf90_noerr) call handle_err(status)

status = nf90_put_att(ofid,varid,'actual_range',actual_range)
if (status /= nf90_noerr) call handle_err(status)

write(0,*)bvarname,actual_range
write(0,*)

end subroutine addvar

! ---------------------

subroutine handle_err(status)

use netcdf

implicit none

! Internal subroutine - checks error status after each netcdf call,
! prints out text message each time an error code is returned. 

integer, intent (in) :: status

if(status /= nf90_noerr) then 
  write(0,*)'NetCDF error: ',trim(nf90_strerror(status))
  stop
end if

end subroutine handle_err

! ---------------------

end module addvarmod