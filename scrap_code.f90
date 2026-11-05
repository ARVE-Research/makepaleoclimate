
!   ! ---
!   
! !   status = nf90_open
! 
!   status = nf90_inq_varid(afid,varname,varid)
!   if (status /= nf90_noerr) call handle_err(status)
! 
!   status = nf90_get_var(afid,varid,anom)
!   if (status /= nf90_noerr) call handle_err(status)
!   
!   if (varname == 'pre') then ! precip anomaly is in mm s-1 (need to convert)
!   
!     do y = 1,ylen
!       do x = 1,xlen
!       
!         anom(x,y,:) = anom(x,y,:) * 86400. * ndaymonth
! 
!       end do
!     end do
!   end if
! 
!   ! --
!  
!   clim = base + anom
!     
!   select case(varname)
!   case ('tmp')
!     scale_factor = 0.1
!     add_offset   = 0.
!   case ('pre')
!     scale_factor = 0.1
!     add_offset   = 3276.7
!     clim = max(clim,0.)
!   case ('cld')
!     scale_factor = 0.01
!     add_offset   = 0.
!     clim = min(max(clim,0.),100.)
!   end select
!   
!   actual_range = [minval(clim),maxval(clim)]
!   
!   ! --
! 
!   var_out = nint((clim - add_offset) / scale_factor,kind=i2)
! 
!   
! end do
! 
! status = nf90_close(afid)
! if (status /= nf90_noerr) call handle_err(status)



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

