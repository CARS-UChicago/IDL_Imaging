pro stxm_event, event

@stxm_common
@img_com
@bsif_common
@camac_common
widget_control, event.id, get_value=value, /clear_event

case event.id of
  button.scan_setup: begin
    read_exit_slit, exit_slit
    sd.exit_slit = exit_slit
    read_wavelength, wavelength, LWB_
    sd.wavelength = wavelength
    read_ens_slit, entrance_slit
    sd.entrance_slit = entrance_slit
   if (XRegistered('SCAN') NE 0) then begin
;	write_message, 'Window already opened'
       ; beep,1
   endif else begin
        widget_control, event.id, show=1, /hourglass
        stxm_scan_setup
    endelse
  endcase

  button.motor_control: begin
    if (XRegistered('MOT') NE 0) then begin
;	write_message, 'Window already opened'
       ; beep,1
   endif else begin
    widget_control, event.id, show=1, /hourglass
    md.arrow = 0
    stxm_mot_setup
   endelse
  endcase

  button.start_scan: begin
;   if (sd.scan_driver_mode NE SGM_MODE) then begin
     widget_control, event.id, /hourglass
       write_scan, "Scan started at "+systime()
       scan_status.image_row=0
       scan_status.overflow(0) = 0
       scan_status.overflow(1) = 0
;; we will have only two channels
;       scan_status.overflow(2) = 0
;      dd.zoom = 1
      stxm_scan
     ; beep, 1
;   endif else begin
;      beep, 2
;      ;   endelse
 endcase


  button.abort_scan: begin
     widget_control, event.id, /hourglass
     abort_scan
     write_scan, "Scan aborted at "+systime()
  endcase

  button.save_file: begin
    if (XRegistered('Get_info') NE 0) then begin
;	write_message, 'Window already opened'
       ; beep,1
    endif else begin
     widget_control, event.id, /hourglass
     get_info
     scan_status.save = 1
    endelse
  endcase

  button.restore: begin
    widget_control, event.id, /hourglass
    file=pickfile(path=pickfile_path,filter=pickfile_filter,/read)
    if file ne "" then begin
;      file = strmid(file,0,strpos(file, '.'))
      widget_control, event.id, /hourglass
; use this for bsif file
;;      read_bsif,file
;;      copy_stxm_sd,sd
;;      valid_sd = sd
; use this for netcdf file
      read_stxm, file, sd
;      copy_stxm_sd,sd
      valid_sd=sd
      dd.rdbsif=1
      md.x_mouse = INVALID_MPOSITION
      stxm_display
    endif
  endcase
    
  button.exit_idl: begin
    save,file='stxmsave.dat',sd,dd
    widget_control, event.id, /hourglass
    widget_control, event.top, /destroy
  endcase

;  button.exit_vms: begin
;    save,file='stxmsave.dat',sd,dd
;    widget_control, event.id, /hourglass
;    exit
;  endcase

  button.motor_mouse: begin
  if(md.x_mouse EQ INVALID_MPOSITION) then begin
	write_message, 'Current mouse position not chosen yet'
	beep, 1
  endif else begin
   widget_control, button.motor_mouse, sensitive=0, /hourglass
   y_mouse=md.y_mouse
   x_mouse=md.x_mouse
   z_mouse=md.z_mouse
; move motors to the actual position and reset pzts.
   z_pzt_offset=sd.z_pzt_offset
   reset_pzt_offset
   if(valid_sd.scan_driver_mode EQ FOCUS_MODE) then begin      
     if ((abs(x_mouse-md.motx_position) LE 3000) and $
	 (abs(z_mouse-md.motz_position) LE 200)) then begin
        move_motor_to, x_motor, x_mouse
        move_motor_to, z_motor, z_mouse
        set_zpiezo, 0, /micron, quiet
	sd.z_pzt_offset = 0
     endif else begin
	beep,2
	write_message,'use stxm_mot_setup to move large scale'
     endelse
   endif else begin
     if((valid_sd.scan_driver_mode EQ STEPPER_MODE) or $
		(valid_sd.scan_driver_mode EQ PZT_MODE)) then begin
       if ((abs(x_mouse-md.motx_position) LE 3000) and $
	 (abs(y_mouse-md.moty_position) LE 3000)) then begin
         move_motor_to, x_motor, x_mouse 
         move_motor_to, y_motor, y_mouse 
       endif else begin
	 beep,2
         write_message,'use stxm_mot_setup to move large scale'
       endelse
     endif
   endelse
   widget_control, button.motor_mouse, sensitive=1 
   write_message, "Motors moved to mouse position"
  endelse
  endcase

  button.pzt_mouse: begin
    widget_control, event.id, /hourglass
    if(md.x_mouse EQ INVALID_MPOSITION) then begin
	write_message, 'Current mouse position not chosen yet'
	beep, 1
    endif else begin
     if((valid_sd.scan_driver_mode EQ FOCUS_MODE) or $
	(valid_sd.scan_driver_mode EQ SGM_MODE)) then begin
      write_message,"Can't move PZTs to mouse on SGM or Focus mode"
     endif else begin
      dx=md.x_mouse-md.motx_position
      dy=md.y_mouse-md.moty_position
      if((abs(dx) LT MAX_PZT_OFFSET) AND (abs(dy) LT MAX_PZT_OFFSET))then begin
      widget_control, button.pzt_mouse, sensitive=0
      stxm_move_queens, dx, dy
      sd.x_pzt_offset = dx
      sd.y_pzt_offset = dy
      write_message, "Moving PZTs to mouse position"
      widget_control, button.pzt_mouse, sensitive=1
      endif else begin
       write_message,'Requested PZT offset is too large'
      endelse
     endelse
    endelse
  endcase

  window.image: begin
    case event.press of
    1: begin            ; Left mouse button
      widget_control, event.id, /hourglass
      plot_row_col
    endcase
    2: begin            ; Center mouse button
      dd.x_center = !D.X_VSIZE/2.
      dd.y_center = !D.Y_VSIZE/2.
      widget_control, event.id, /hourglass
      stxm_display
    endcase
    4: begin
      dd.x_center = dd.x_center - event.x + !D.X_VSIZE/2.
      dd.y_center = dd.y_center - event.y + !D.Y_VSIZE/2.
      widget_control, event.id, /hourglass
      stxm_display
    endcase
    else: begin
    endcase
   endcase
  endcase

  button.copy_iti: begin
    widget_control, event.id, /hourglass
    widget_control, button.copy_iti, sensitive=0
    write_message, "Submitting iti display task to bnlx1"
    wset, window.image1
; to simplify, read the center 640 by 480 for ITI display.
;;    xoffset=(800-640)/2
;;   yoffset=(800-480)/2
;;    tvrd_buffer = tvrd(xoffset,yoffset,640, 480)
;;    tvlct,rct,gct,bct,/get
; Well, IDL does variable length records which can each hold
; only 32767 bytes.  But that's only 51 rows worth of data!
; So what we'll do is we'll write out 48 rows at a time, and
; have netiti read 48 rows at a time.  Also, the way that
; IDL does a writeu in VMS is to use variable length records,
; and after each record a byte of some sort is written. So
; we need to read 48*640 bytes for 48 rows, a byte of garbage, and
; so on.  This is done by NETITI.C, which is called by NETITI_CALL.FOR
;;    openw,netunit,/get_lun, $
;;	'bnlx1l$dka200:[micros.data]cur_image.dat;1', /supersede
;;    for rowgroup=0,9 do begin
;;	startindex=long(rowgroup)*long(48)*long(640)
;;	endindex=long(rowgroup+1)*long(48)*long(640)-long(1)
;;	writeu, netunit, tvrd_buffer(startindex:endindex)
;;    endfor
; write the lookup table info
;;    lutnum=byte(n_elements(rct))
;;    writeu,netunit,lutnum
;;    writeu,netunit,rct
;;    writeu,netunit,gct
;;    writeu,netunit,bct
; close and do
;;    free_lun, netunit
;;    spawn,'run netiti_call',/nowait
    copy_to_iti, iti_bytarr_image
    widget_control, button.copy_iti, sensitive=1
  endcase

; print out at the laser printer

  button.print:begin
     widget_control, event.id, /hourglass
     if (dd.rdbsif EQ 1) then begin ;  if it's a restored file
       title=string(valid_sd.file_name)
       stxm_psprint, valid_sd, title
     endif else begin
       if (scan_status.save EQ 0) then begin	; if this is a scan not saved
	 title='Might be '+string(valid_sd.file_name)+'!C (before save)'
         stxm_psprint, valid_sd, title
       endif else begin
	 title=string(current_filename)
         stxm_psprint, valid_sd, title
       endelse	
     endelse
  endcase

  button.save_as_I0: begin
    widget_control, event.id, /hourglass
    I0_data=image_data
    I0_sd=sd
  endcase

  button.update_displ:begin
    widget_control, event.id, /hourglass
;    update_display
     stxm_display
  endcase

  button.new_lut: begin
     widget_control, event.id, /hourglass
     xloadct
  endcase

  button.display_i: begin
     print, 'Now displaying I'
     widget_control, event.id, /hourglass
    dd.displayed_image = DISPLAY_I
    dd.low_limit=min(image_data(*,*,0))
    dd.high_limit=max(image_data(*,*,0))
    widget_control, text.low, set_value=string(dd.low_limit, format='(e7.1)')
    widget_control, text.high, set_value=string(dd.high_limit,format='(e7.1)')
    stxm_display
  endcase

  button.display_clock: begin
    print, 'Now displaying clock'
     widget_control, event.id, /hourglass
    dd.displayed_image = DISPLAY_CLOCK
    dd.low_limit=min(image_data(*,*,1))
    dd.high_limit=max(IMAGE_DATA(*,*,1))
    widget_control,text.low, set_value=string(dd.low_limit,format='(e7.1)')
    widget_control,text.high,set_value=string(dd.high_limit,format='(e7.1)')
    stxm_display
  endcase

  button.display_i_clock: begin
     widget_control, event.id, /hourglass
    dd.displayed_image = DISPLAY_I_CLOCK
    dd.low_limit=min(image_data(*,*,0)/float(image_data(*,*,1))* $
		sd.clock_frequency)
    dd.high_limit=max(image_data(*,*,0)/float(image_data(*,*,1))* $
		sd.clock_frequency)
    widget_control,text.low,set_value=string(dd.low_limit,format='(e7.1)')
    widget_control,text.high,set_value=string(dd.high_limit,format='(e7.1)')
    stxm_display
  endcase

  button.display_i0: begin
     widget_control, event.id, /hourglass
    dd.displayed_image = DISPLAY_I0
    if (I0_data(0,0,0) NE 0) then begin 
    dd.low_limit=min(I0_data(*,*,0)/float(I0_data(*,*,1))* $
		I0_sd.clock_frequency)
    dd.high_limit=max(I0_data(*,*,0)/float(I0_data(*,*,1))* $
		I0_sd.clock_frequency)
    widget_control,text.low,set_value=string(dd.low_limit,format='(e7.1)')
    widget_control,text.high,set_value=string(dd.high_limit,format='(e7.1)')
    stxm_display
    endif
  endcase

  button.display_i_I0: begin
     widget_control, event.id, /hourglass
   if(valid_sd.scan_driver_mode EQ SGM_MODE) then begin
   if(I0_data(0,0,0) NE 0) then begin
    if (I0_sd.scan_width EQ sd.scan_width) then begin
	
     dd.displayed_image = DISPLAY_I_I0
     I0_spect = I0_data(*,*,0)/float(I0_data(*,*,1))*I0_sd.clock_frequency
     I_spect=image_data(*,*,0)/float(image_data(*,*,1))*sd.clock_frequency
     displayed_data=-alog(I_spect/I0_spect)
     dd.low_limit=min(displayed_data)
     dd.high_limit=max(displayed_data)
     widget_control,text.low,set_value=string(dd.low_limit,format='(e7.1)')
     widget_control,text.high,set_value=string(dd.high_limit,format='(e7.1)')
     stxm_display
    endif
   endif
   endif 
  endcase

  slider.zoom: begin
    dd.zoom = value
    x_zoom = dd.zoom
    y_zoom = dd.zoom
    widget_control, event.id, /hourglass
    if (scan_status.scan eq 1) then begin
      stxm_display
    endif else begin
      wset, window.image1
      img_display
        bottom=(!D.x_vsize/2-(valid_sd.scan_height*dd.zoom/2)-30)/float(!D.x_vsize)
        left=(!d.y_vsize/2-(valid_sd.scan_width*dd.zoom/2))/float(!D.y_vsize)
        if (bottom le 0) then bottom=0.05
    ; calculate a default scale bar length of a fifth the image width
        sblen=valid_sd.scan_width*(dd.zoom>1)/float(!d.x_vsize) 
        right=left+valid_sd.scan_width*(dd.zoom>1)/float(!d.x_vsize)*0.25
        barsize = valid_sd.scan_width * valid_sd.x_step_size
        len = nint(barsize * 0.2) > 1
        barlen = len/barsize * sblen
        bar=bytarr(barlen*!d.x_vsize>1,0.01*!d.y_vsize>1)
        bar=bar+255
        tv, bar, /norm, left, bottom ,xsize=barlen, ysize=0.01 
        xyouts,/norm,right,bottom,strtrim(string(len, $
		format='(f6.1)'),2)+' um', $
	   color=!p.color
    endelse
  endcase
  
  slider.low_display: begin
    dd.low = value
    widget_control, event.id, /hourglass
    min=dd.low*(dd.high_limit-dd.low_limit)*0.01 + $
		dd.low_limit
    max=(dd.high)*(dd.high_limit)*0.01
    widget_control, label2.low, set_value=string(min, format='(e7.1)')
    widget_control, label2.high, set_value=string(max, format='(e7.1)')
    stxm_display
  endcase

  slider.high_display: begin
    dd.high = value
    widget_control, event.id, /hourglass
    min=dd.low*(dd.high_limit-dd.low_limit)*0.01 + $
		dd.low_limit
    max=(dd.high)*(dd.high_limit)*0.01
    widget_control, label2.low, set_value=string(min, format="(e7.1)")
    widget_control, label2.high, set_value=string(max, format="(e7.1)")
    stxm_display
  endcase

  text.low: begin
    widget_control, event.id, /hourglass
    dd.low_limit=float(value(0))
    widget_control, text.low, set_value=string(dd.low_limit, $
	format="(f8.1)")
    stxm_display
  endcase
 
  text.high: begin
    widget_control, event.id, /hourglass
    dd.high_limit=float(value(0))
    widget_control, text.high, set_value=string(dd.high_limit,$
	format="(f8.1)")
    stxm_display
  endcase

  button.help: begin
     widget_control, event.id, /hourglass
     stxm_help
  endcase
  
  button.plot_on: begin
     scan_status.plot = 0
  endif

  button.plot_off: begin
     scan_status.plot = 1
  endif

  else: write_message, "Unknown button pressed"
endcase

end



pro stxm_display
@bsif_common
@img_com
@stxm_common

wset, window.image1
case dd.displayed_image of
  DISPLAY_I: begin
	img_scl, image_data(*,*,0), zoom=dd.zoom, $
	min=dd.low*(dd.high_limit-dd.low_limit)*0.01 + $
		dd.low_limit, $
	max=(dd.high)*(dd.high_limit)*0.01, $
        center=[dd.x_center, dd.y_center]
        widget_control, label2.low_limit, set_value= $
		string(min(image_data(*,*,0)),format="(f10.3)")
	widget_control, label2.high_limit, set_value= $
		string(max(image_data(*,*,0)),format="(f10.3)")
  endcase


  DISPLAY_CLOCK: begin
        dd.low_limit=min(image_data(*,*,1))
	dd.high_limit=max(image_data(*,*,1))
        print, 'now displaying clock'
	img_scl, image_data(*,*,1), zoom=dd.zoom, $
	min=(dd.low)*(dd.high_limit-dd.low_limit)*0.01 + $
		dd.low_limit, $
	max=(dd.high)*dd.high_limit*0.01, $
        center=[dd.x_center, dd.y_center]

        widget_control, label2.low_limit, set_value= $
		string(min(image_data(*,*,1)),format="(f10.3)")
        widget_control, label2.high_limit, set_value= $
		string(max(image_data(*,*,1)),format="(f10.3)")
   endcase


  DISPLAY_I_CLOCK: begin
        low_data=min(image_data(*,*,0)/float(image_data(*,*,1))* $
		sd.clock_frequency)
        high_data=max(image_data(*,*,0)/float(image_data(*,*,1))* $
		sd.clock_frequency)
        img_scl, float(image_data(*,*,0))/(image_data(*,*,1))* $
		sd.clock_frequency, zoom=dd.zoom,$
	min=(dd.low)*(dd.high_limit-dd.low_limit)* 0.01+ $
		dd.low_limit,$
	max=(dd.high)*dd.high_limit*0.01, $
        center=[dd.x_center, dd.y_center]

        widget_control, label2.low_limit, set_value=string(low_data,$
                        format="(f10.3)")
        widget_control, label2.high_limit, set_value=string(high_data,$
                        format="(f10.3)")
  endcase

  DISPLAY_I0: begin
       if (I0_data(0,0,0) NE 0) then begin
        low_data=min(I0_data(*,*,0)/float(I0_data(*,*,1))* $
		I0_sd.clock_frequency)
        high_data=max(I0_data(*,*,0)/float(I0_data(*,*,1))* $
		I0_sd.clock_frequency)
        img_scl, float(I0_data(*,*,0))/(I0_data(*,*,1))* $
		I0_sd.clock_frequency, zoom=dd.zoom,$
	min=(dd.low)*(dd.high_limit-dd.low_limit)* 0.01+ $
		dd.low_limit,$
	max=(dd.high)*dd.high_limit*0.01, $
        center=[dd.x_center, dd.y_center]

        widget_control, label2.low_limit, set_value=string(low_data,$
                        format="(f10.3)")
        widget_control, label2.high_limit, set_value=string(high_data,$
                        format="(f10.3)")
    endif
  endcase

  DISPLAY_I_I0: begin
   if (valid_sd.scan_driver_mode EQ SGM_MODE) then begin
    if (I0_data(0,0,0) NE 0) then begin
     if (I0_sd.scan_width EQ sd.scan_width) then begin
	I0_spect=I0_data(*,*,0)/float(I0_data(*,*,1))*I0_sd.clock_frequency
	I_spect=image_data(*,*,0)/float(image_data(*,*,1))*sd.clock_frequency
	displayed_spect=-alog(I_spect/I0_spect)
        low_data=min(displayed_spect)
        high_data=max(displayed_spect)
        img_scl, displayed_spect, zoom=dd.zoom,$
	min=(dd.low)*(dd.high_limit-dd.low_limit)* 0.01+ $
		dd.low_limit,$
	max=(dd.high)*dd.high_limit*0.01, $
        center=[dd.x_center, dd.y_center]

        widget_control, label2.low_limit, set_value=string(low_data,$
                        format="(f10.3)")
        widget_control, label2.high_limit, set_value=string(high_data,$
                        format="(f10.3)")
    endif
   endif
   endif
  endcase

endcase

if (valid_sd.scan_driver_mode eq SGM_MODE) then begin
  plot_row_col
endif
    
        bottom=(!D.x_vsize/2-(valid_sd.scan_height*(dd.zoom>1)/2)-30)/float(!D.x_vsize)
        left=(!d.y_vsize/2-(valid_sd.scan_width*(dd.zoom>1)/2))/float(!D.y_vsize)
        if (bottom le 0) then bottom=0.05
    ; calculate a default scale bar length of a fifth the image width
        sblen=valid_sd.scan_width*(dd.zoom>1)/float(!d.x_vsize) 
;    print, sblen    
        right=left+valid_sd.scan_width*(dd.zoom>1)/float(!d.x_vsize)*0.25
        barsize = valid_sd.scan_width * valid_sd.x_step_size
        len = nint(barsize * 0.2) > 1
        barlen = len/barsize * sblen
        bar=bytarr(barlen*!d.x_vsize>1,0.01*!d.y_vsize>1)
        bar=bar+255
        tv, bar, /norm, left, bottom ,xsize=barlen, ysize=0.01 
        xyouts,/norm,right,bottom,strtrim(string(len, $
		format='(f6.1)'),2)+' um', $
	   color=!p.color

end

pro write_message, string
@stxm_common
widget_control, text.messages, set_value=string, /append
end

pro write_scan, string
@stxm_common
widget_control, text.scan_status, set_value=string, /append
end


pro PLOT_ROW_COL, dummy

@bsif_common
@img_com
@keypad_defs
@stxm_common

wset, window.image1
cursor, /device, x, y,/nowait
dc_to_ic, x, y, xc, yc
xc = xc > 0
yc = yc > 0
xc = xc < (valid_sd.scan_width-1)
yc = yc < (valid_sd.scan_height-1)

case  valid_sd.scan_driver_mode of
   STEPPER_MODE : begin
     x_mouse=xc
     y_mouse=yc
   endcase
   
   PZT_MODE : begin
     x_mouse=xc
     y_mouse=yc
   endcase

   FOCUS_MODE: begin
     x_mouse=xc
     y_mouse=yc
    endcase

   SGM_MODE: begin
     x_mouse=xc
     y_mouse=0
   endcase
    else: return
endcase

if((valid_sd.scan_driver_mode EQ PZT_MODE) OR  $
	(valid_sd.scan_driver_mode EQ STEPPER_MODE)) then begin
   widget_control, label.x_mouse, set_value=string(x_mouse)
   widget_control, label.y_mouse, set_value=string(y_mouse)
   widget_control, label.z_mouse, set_value='0'
   md.x_mouse=valid_sd.x_start+valid_sd.x_step_size*x_mouse
   md.y_mouse=valid_sd.y_start+valid_sd.y_step_size*y_mouse
   md.z_mouse=valid_sd.focus_position
   md.lam_mouse=valid_sd.wavelength
endif
if(valid_sd.scan_driver_mode EQ FOCUS_MODE) then begin
   widget_control, label.x_mouse, set_value=string(x_mouse)
   widget_control, label.y_mouse, set_value='0'
   widget_control, label.z_mouse, set_value=string(y_mouse)
   md.x_mouse=valid_sd.x_start + valid_sd.x_step_size*x_mouse
   md.y_mouse=valid_sd.y_center
   md.z_mouse=valid_sd.z_start + valid_sd.z_step_size*y_mouse+ $
		sd.z_pzt_offset
   md.lam_mouse=valid_sd.wavelength
endif
if(valid_sd.scan_driver_mode EQ SGM_MODE) then begin
   widget_control, label.x_mouse, set_value=string(x_mouse)
   widget_control, label.y_mouse, set_value='0'
   widget_control, label.z_mouse, set_value='0'
   md.x_mouse=valid_sd.x_center
   md.y_mouse=valid_sd.y_center
   md.z_mouse=valid_sd.z_start+valid_sd.z_step_size*x_mouse
   md.lam_mouse=valid_sd.initial_lambda+valid_sd.sgm_step_size*x_mouse
endif
widget_control, label.x_mousem, set_value=string(md.x_mouse)
widget_control, label.y_mousem, set_value=string(md.y_mouse)
widget_control, label.z_mousem, set_value=string(md.z_mouse)
widget_control, label.lam_mousem, set_value=string(md.lam_mouse)
  dc_to_ic, 0, 0, x_min, y_min
  x_min = max([0,x_min])
  y_min = max([0,y_min])
  xc=max([0,x_mouse])
  yc=max([0,y_mouse])
  dc_to_ic, !D.x_vsize-1, !D.y_vsize-1, x_max, y_max
  x_max = min([x_max,image_width-1])
  y_max = min([y_max,image_height-1])
  xc=min([xc, x_max])
  yc=min([yc, y_max])

if scan_status.plot eq 0 then begin
  x_axis = findgen(image_width)
  x_inc = float((valid_sd.x_stop - valid_sd.x_start))/(image_width-1)
  if (x_inc EQ 0) then x_inc = 1.
  x_axis = float(valid_sd.x_start) + x_axis*x_inc
  y_axis = findgen(image_height)
  y_inc = float((valid_sd.y_stop - valid_sd.y_start))/(image_height-1)
  if (y_inc EQ 0) then y_inc = 1.
  y_axis = float(valid_sd.y_start) + y_axis*y_inc
  diff = white_level - black_level
  crange = [(black_level - 0.1*diff), (white_level + 0.1*diff)]
  diff = x_axis(x_max) - x_axis(x_min)
  xrange = [(x_axis(x_min) - 0.05*diff), (x_axis(x_max) + 0.05*diff)]
  diff = y_axis(y_max) - y_axis(y_min)
  yrange = [(y_axis(y_min) - 0.05*diff), (y_axis(y_max) + 0.05*diff)]

;  Turning off tick labels speeds the plots up dramatically
  !x.tickname = ' '
  !y.tickname = ' '
  !p.position = [0,0,1,1]

  if (x_min ne x_max) then begin
   wset, window.x_profile
   plot, x_axis(x_min:x_max), raw_data(x_min:x_max, yc), $
         xrange=xrange, yrange=crange, xstyle=1, psym=10
   xline=[x_axis(xc),x_axis(xc)]
   yline=crange
   oplot,xline,yline
  endif

  if (y_min ne y_max) then begin
   wset, window.y_profile
   plot, raw_data(xc, y_min:y_max), y_axis(y_min:y_max), $
         xrange=crange, yrange=yrange, ystyle=1, psym=10
   xline=crange
   yline=[y_axis(yc),y_axis(yc)]
   oplot,xline,yline
  endif

  !x.tickname = ''
  !y.tickname = ''
  !p.position = 0

endif
widget_control, label.data, set_value=string((raw_data(xc,yc)), $
					format="(f8.3)")
return
end

pro back_bck,topid
@stxm_common
@bsif_common
@img_com
@camac_common
if (scan_status.scan EQ 1) then begin
 if(scan_status.image_row LT sb.last_valid_row) then begin
   ; check whether new data collected
   ;   print,'sb.last_valid_row=',  sb.last_valid_row
   line_display

 endif
   if (sb.scan_status ne SCAN_ACTIVE) then begin
        if (sd.auto_shutter) then pd_shutter,/close
        line_display    ; Make sure last line of data was displayed
        beep,1
        if sb.scan_status eq SCAN_COMPLETE then begin
           write_scan, 'Scan completed normally'
        endif else if sb.scan_status eq SCAN_ERROR then begin
           write_scan, 'Scan terminated due to error'
        endif else if sb.scan_status eq SCAN_ABORTED then begin
           write_scan, 'Scan aborted by operator'
        endif
        move_motor_back
	scan_status.scan=0

        if(scan_status.overflow(0) EQ 1) then begin
		write_scan,'Possible overflow in transmitted flux'
	endif
	if(scan_status.overflow(1) EQ 1) then begin
		write_scan,'Possible overflow in clock pulses'
        endif
        ; Change widget sensitivities
;        widget_control, button.scan_setup, sensitive=1
        widget_control, button.motor_control, sensitive=1
        widget_control, button.start_scan, sensitive = 1
	widget_control, button.restore, sensitive = 1
	widget_control, button.exit_idl, sensitive = 1
;	widget_control, button.exit_vms, sensitive = 1
        widget_control, button.abort_scan, sensitive = 0
        ; Redisplay entire image so zoom can just call img_display
        stxm_display
        widget_control, button.motor_mouse, sensitive = 1
	widget_control, button.pzt_mouse, sensitive = 1
   endif
wait,0.1
endif else begin
 time=systime(1)
 if((time-prevtime) GT 1.) then begin
   get_mot_pos, x_motor, motx_position
   md.motx_position=motx_position
   get_mot_pos, y_motor, moty_position
   md.moty_position=moty_position
   get_mot_pos, z_motor, motz_position
   md.motz_position=motz_position

; if the motor is still moving, notify the motor setup to desensitize motor
; movement and sensitize abort motor button
;   print, md.motx_position, motx_pos_o
   if ((abs(md.motx_position- motx_pos_o) GE 0.5) or $
	(abs(md.moty_position - moty_pos_o) GE 0.5) or $
	(abs(md.motz_position - motz_pos_o) GE 0.1)) then begin
	md.mot_status=1
   endif else begin
        md.mot_status=0
   endelse 

   get_stxm_status, i, i0, sd.clock_frequency, ring_current
   sd.ring_current= float(ring_current)
   widget_control, label.x_motor, $
        set_value=string(md.motx_position,format="(f10.2)")
   widget_control, label.y_motor, $
        set_value=string(md.moty_position, format="(f10.2)")
   widget_control, label.z_motor, $
        set_value=string(md.motz_position, format="(f10.2)")
   widget_control, label.x_pzt, $
        set_value=string(sd.x_pzt_offset, format="(f10.3)")
   widget_control, label.y_pzt, $
        set_value=string(sd.y_pzt_offset, format="(f10.3)")
   widget_control, label.z_pzt, $
        set_value=string(sd.z_pzt_offset, format="(f10.2)")

   widget_control, label.ring_current, $
        set_value=string(sd.ring_current, format='(I4)')+'ma'
   widget_control, label.i, set_value=string(i)
   widget_control, label.i0, set_value=string(i0)
   prevtime = time
; remember the current motor position for monitoring motor movement
   motx_pos_o = md.motx_position
   moty_pos_o = md.moty_position
   motz_pos_o = md.motz_position
 endif else wait, 0.2
endelse
end


pro stxm,user_restore_file

@stxm_common
@bsif_common
@img_com
@keypad_defs
@camac_common
if n_elements(sd) eq 0 then stxm_init_test
;init_common
font_init
if (n_elements(user_restore_file) eq 0) then user_restore_file=''
stxm_startup,user_restore_file

base = widget_base(title="STXM")
; widget_control, base, default_font=LARGE_FONT
col1 = widget_base(base, xsize=250, ysize=950, column=1)
;col2 = widget_base(base, xoffset=260, xsize=512, ysize=800)
col2 = widget_base(base, xoffset=260, xsize=512, ysize=950)
;col3 = widget_base(base, xoffset=780, xsize=250, ysize=800)
col3  = widget_base(base, xoffset=780, xsize=250, ysize=950)
window.image = widget_draw(col2, /scroll, /button_events, $
                           xsize=800, ysize=800, $
                           x_scroll_size=512, y_scroll_size=512, $
                           yoffset=200, retain=2)
window.x_profile  = widget_draw(col2, xsize=512, ysize=150, yoffset=720)
window.y_profile  = widget_draw(col3, xsize=200, ysize=512, yoffset=200)

w2 = widget_base(col1, column=2)
button.scan_setup  = widget_button(w2, value='Scan Setup', font=LARGE_FONT)
button.motor_control = widget_button(w2, value='Motor Setup', font=LARGE_FONT)
;button.SGM_setup   = widget_button(w2, value='SGM Setup', font=LARGE_FONT)
button.start_scan  = widget_button(w2, value='Start Scan', font=LARGE_FONT)
button.abort_scan  = widget_button(w2, value='Abort Scan', font=LARGE_FONT)
widget_control, button.abort_scan, sensitive=0
button.save_file        = widget_button(w2, value='SaveFile', font=LARGE_FONT)
button.restore     = widget_button(w2, value='RestoreFile', font=LARGE_FONT)
;button.exit_VMS    = widget_button(w2, value="Exit to VMS", font=LARGE_FONT)
button.copy_iti    = widget_button(w2, value='Copy to ITI', font=LARGE_FONT)
button.print	   = widget_button(w2, value='Print', font=LARGE_FONT)
button.save_as_I0  = widget_button(w2, value='Save_as_I0', font=LARGE_FONT)

; turn off this button for now since vax4000 can't support ITI board.
;widget_control, button.copy_iti, sensitive=0

;button.info	   = widget_button(w2, value="Info")

xmenu,            ["I", "Clock", "I/Clock", "-ln(I/I0)", "I0"], $
                  col1, column=3, /exclusive, select=dd.displayed_image, $
                  /no_release, title="Displayed Image", buttons=b
button.display_i        = b(0)
button.display_clock    = b(1)
button.display_i_clock  = b(2)
button.display_i_i0	= b(3)
button.display_i0	= b(4)
slider.zoom     = widget_slider(col1, title="Zoom", min=1, max=10, $
		  value=dd.zoom)

w1=widget_base(col1, column=1, /frame)
w2=widget_label(w1, value='image limits')
w2=widget_base(w1, /row, /frame)
label2.low_limit=widget_label(w2, value='0.0', /frame)
label2.high_limit=widget_label(w2, value='1.0', /frame)
w1              = widget_base(col1, column=1, /frame)
w2              = widget_label(w1, value="Display Limits")
; change display limit, dd.low_limit is the low display limit
w2=widget_base(w1, /row, /frame)
w3=widget_base(w2, /column)
text.low=widget_text(w3, /editable,xsize=6, value=string(dd.low_limit))
label2.low=widget_label(w3, xsize=6, value=string(dd.low_limit))
slider.low_display = widget_slider(w2, title="Low( %)", min=1, max=100,$
	xsize=110)
w2=widget_base(w1, /row, /frame)
w3=widget_base(w2, /column)
text.high=widget_text(w3, /editable,xsize=6, value=string(dd.high_limit))
label2.high=widget_label(w3, xsize=6, value=string(dd.high_limit))
slider.high_display = widget_slider(w2, title="High( %)", min=1,max=100,$
	xsize=110)
w1              = widget_base(col1, column=1, /frame)
button.new_lut  = widget_button(w1, value="Change color table")
button.update_displ=widget_button(w1, value='update display')
                   

w1      = widget_base(col1, /row)
w2      = widget_base(w1, /column)
w3      = widget_label(w2, value="  Count Rate")
w3      = widget_base(w2, /row)
w4      = widget_label(w3, value="I ")
label.i = widget_label(w3, /frame, xsize=8, value='1000.2')
w3      = widget_base(w2, /row)
w4      = widget_label(w3, value="I0")
label.i0 = widget_label(w3, /frame, xsize=8, value='1500.1')
w3	=widget_base(w2, /row)
w4	=widget_label(w3, value="Ring Current")
label.ring_current = widget_label(w3, /frame, xsize=25, value='140mA')
button.motor_mouse = widget_button(w2, value="Move Motors to Mouse",$
			font=LARGE_FONT)
button.pzt_mouse   = widget_button(w2, value="Move PZTs to Mouse", $
			font=LARGE_FONT)
button.exit_idl    = widget_button(w2, value='Exit to IDL',ysize=15)
w1	= widget_base(col2, /row)
w2      = widget_base(w1, column=1, /frame)
w3      = widget_base(w2, column=5)
w4      = widget_label(w3, value=" ", xsize=10,ysize=1)
w4      = widget_label(w3, value="Motor",xsize=10,ysize=1)
w4      = widget_label(w3, value="PZT",xsize=10, ysize=1)
w4      = widget_label(w3, value="Mouse",xsize=10, ysize=1)
w4      = widget_label(w3, value="(microns)",xsize=10, ysize=1)

w4      = widget_label(w3, value="X",xsize=10,ysize=1)
label.x_motor = widget_label(w3, value='0.0', /frame,xsize=10,ysize=1)
label.x_pzt   = widget_label(w3, value='0.0', /frame,xsize=10,ysize=1)
label.x_mouse = widget_label(w3, value='0.0', /frame,xsize=10,ysize=1)
label.x_mousem	      = widget_label(w3, value='0.0', /frame,xsize=10,ysize=1)

w4      = widget_label(w3, value="Y",xsize=10,ysize=1)
label.y_motor = widget_label(w3, value='0.0', /frame,xsize=10,ysize=1)
label.y_pzt = widget_label(w3, value='0.0', /frame,xsize=10,ysize=1)
label.y_mouse = widget_label(w3, value='0.0', /frame,xsize=10,ysize=1)
label.y_mousem	      = widget_label(w3, value='0.0', /frame,xsize=10,ysize=1)

w4      = widget_label(w3, value="Z",xsize=10,ysize=1)
label.z_motor = widget_label(w3, value='5.0', /frame,xsize=10,ysize=1)
label.z_pzt = widget_label(w3, value='0.0',/frame,xsize=10,ysize=1)
label.z_mouse = widget_label(w3, value='0.0', /frame,xsize=10,ysize=1)
label.z_mousem = widget_label(w3, value='0.0',/frame,xsize=10,ysize=1)

button.help = widget_button(w3, value= "HELP", xsize=5, /frame,ysize=1)
w4 = widget_label(w3, value='Lambda:',xsize=10,ysize=1)
label.lam_mousem = widget_label(w3, value='0.0',/frame,xsize=10,ysize=1)
w4      = widget_label(w3, value="Data:",xsize=10,ysize=1)
label.data      = widget_label(w3, value='0.0', /frame,xsize=10,ysize=1)

;w4 = widget_label(w3, value="plot_line")
;w4=widget_base(w3, /row, /exclusive)
;button.plot_on = widget_button(w4, value="On", uvalue="Plot_on", /no_release)
;button.plot_off = widget_button(w4, value="Off", uvalue="Plot_off")
w1              = widget_base(col3, column=1, /frame)
w2              = widget_label(w1, value="Scan status")
label.scan_status=widget_label(w1,value='Scan Status',/frame) 
text.scan_status = widget_text(w1, /scroll, xsize=25, ysize=4)
w1              = widget_base(col3, column=1, /frame, yoffset=720)
w2              = widget_label(w1, value="Messages")
text.messages   = widget_text(w1, /scroll, xsize=25, ysize=4)
;button.help = widget_button(w1, value= "HELP", /frame)

widget_control, base, /REALIZE, /clear_events

widget_control, window.image, get_value=temp
window.image1=temp
widget_control, window.x_profile, get_value=temp
window.x_profile=temp
widget_control, window.y_profile, get_value=temp
window.y_profile=temp

stxm_erase_data
stxm_display
device,/cursor_original
xmanager, "STXM", base,background='back_bck', group_leader=group  
end
