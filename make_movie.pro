pro make_movie, vol, min=min, max=max, wait=wait, scale=scale, index=index, $
                start=start, stop=stop, step=step, mpeg_file=mpeg_file, $
                jpeg_file=jpeg_file, label=label, quality=quality, color=color

;+
; NAME:
;   MAKE_MOVIE
;
; PURPOSE:
;   This procedure plays a 3-D array as a movie either:
;       1) On the screen
;       2) To an MPEG file on disk
;       2) To a series of JPEG files on disk
;
; CATEGORY:
;   Image display.
;
; CALLING SEQUENCE:
;   MAKE_MOVIE, Volume
;
; INPUTS:
;   Volume: A 3-D array of any numeric data type
;
; KEYWORD PARAMETERS:
;   MIN:
;       The minimum value of the data display range.  This sets the low end
;       of the grey scale range.  All values less than this will be appear
;       black when using a linear grey scale.  The default is the minimum value
;       of the data in the entire Volume array.
;
;   MAX:   
;       The maximum value of the data display range.  This sets the high end
;       of the grey scale range.  All values greater than this will be appear
;       white when using a linear grey scale.  The default is the maximum value
;       of the data in the entire Volume array.
;
;   SCALE:   
;       A scale factor to increase or decrease the size of the displayed images
;       relative to the size of the Volume array.  SCALE=2 will double the 
;       size of the displayed images, SCALE=3 will display the images at 3 
;       times their normal size, etc.  SCALE=-2 will display the images at 
;       half their normal size, SCALE=-3 one-third their normal size, etc. 
;       The default is 1, i.e. no scaling. 
;
;   INDEX:
;       This keyword controls which dimension of the Volume array is animated.
;       Allowed values are 1, 2 and 3.  The following table shows the result
;       of setting this keyword:
;
;           INDEX   frame[i]
;             1   Volume[i,*,*]
;             2   Volume[*,i,*]
;             3   Volume[*,*,i]
;       The default is 3, i.e. the last dimension of the Volume array is the
;       one which is animated.
;
;   START:   
;       The first frame to display.  The default is 0.
;
;   STOP:
;       The last frame to display.  The default is the highest value of the
;       selected index.
;
;   STEP:
;       The array increment from one frame to the next.  The default is 1.
;
;   WAIT:
;       The delay time from one frame to the next in floating point seconds.
;       The default is 0., i.e. frames are diplayed as fast as possible.
;       This keyword has no effect if the JPEG_FILE or MPEG_FILE keywords are used.
;
;   MPEG_FILE:
;       The name of an MPEG file to which the output should be written.  If
;       this keyword is used then this procedure does not display its output on
;       the screen but rather creates an MPEG file.
;       NOTE: IDL 5.4 requires a special (free) license upgrade to write MPEG
;       files.  See the RSI Web site for details.
;
;   JPEG_FILE:
;       The base name of an JPEG file to which the output should be written.  If
;       this keyword is used then this procedure does not display its output on
;       the screen but rather creates a series of JPEG files. A sequence number and
;       the extension .jpg are appended to this base name. The file names are thus
;       of the form 'my_jpeg_name_0001.jpg', 'my_jpeg_name_0002.jpg', etc.  The system
;       variable !order is used to determine whether the files are written bottom to
;       top or top to bottom.
;
;   QUALITY:
;       The quality factor for JPEG and MPEG files.
;       See the help for WRITE_JPEG for more information on using this 
;       keyword for JPEG files.
;       See the help for MPEG_OPEN for more information on using this 
;       keyword for MPEG files.  For MPEG files this keyword is only allowed
;       for IDL 5.4 and higher.
;       
;       Default=90.
;
;   COLOR
;       A flag which indicates that the JPEG or MPEG output files should be in color,
;       using the current color table, rather than black and white.
;
;   LABEL:
;       A flag which indicates that the current frame number should be drawn
;       as a label on the screen.  This is useful for determining which frames
;       interesting features appear in, for use later with IMAGE_DISPLAY
;       or other routines.
;
; OUTPUTS:
;   This procedure does not return any output to IDL.  It displays images on
;   the screen using the IDL TV procedure, or writes output to disk if MPEG_FILE 
;   or JPEG_FILE is used.
;
; SIDE EFFECTS:
;   Displays images on the screen using the IDL TV procedure, or writes output 
;   to disk if MPEG_FILE or JPEG_FILE is used.
;
; PROCEDURE:
;   This procedure converts each frame to an 8-bit array using the IDL BYTSCL
;   function and then either displays it on the screen using the TV procedure,
;   writes it to an MPEG file, or write it to a series of JPEG files.
;
; EXAMPLE:
;   ; Display an array as a movie scanning through the first dimension of the
;   ; array, forcing the black level to be 1000, and waiting 0.2 seconds
;   ; between frames
;   MAKE_MOVIE, Volume, index=1, wait=.2, min=1000
;
;   ; Create an MPEG movie of an array scanning through the last dimension 
;   ; (default) of the array, forcing the white level to be 10000.
;   MAKE_MOVIE, Volume, max=10000, mpeg_file='movie1.mpeg'
;
;   ; Create a series of JPEG files of an array scanning through the last dimension 
;   ; (default) of the array, forcing the white level to be 10000.
;   MAKE_MOVIE, Volume, max=10000, jpeg_file='my_jpegs'
;
; MODIFICATION HISTORY:
;   Written by:     Mark Rivers, April 26, 1999.  Merged previous routines
;                   TOMO_MOVIE and WRITE_MPEG
;   June 27, 2000  MLR  Changed position of label slightly for legibility
;   Sep. 29, 2000  MLR  Added JPEG_FILE and QUALITY keywords.
;   Feb. 27, 2001  MLR  Added COLOR keyword for color JPEG and MPEG files.  
;                       Made QUALITY keyword apply to MPEG files if the IDL version 
;                       is 5.4 or higher.
;-

if (n_elements(min) eq 0) then min=min(vol)
if (n_elements(max) eq 0) then max=max(vol)
if (n_elements(wait) eq 0) then wait=0
if (n_elements(scale) eq 0) then scale=1
if (n_elements(index) eq 0) then index=3

nx = n_elements(vol[*,0,0])
ny = n_elements(vol[0,*,0])
nz = n_elements(vol[0,0,*])
case index of
   1: begin
      ncols=ny
      nrows=nz
      nframes=nx
   end
   2: begin
      ncols=nx
      nrows=nz
      nframes=ny
   end
   3: begin
      ncols=nx
      nrows=ny
      nframes=nz
   end
   else: message, 'INDEX must be in the range 1-3'
endcase

if (scale gt 1) then begin
    last_col = ncols - 1
    last_row = nrows - 1
    ncols = ncols * fix(scale)
    nrows = nrows * fix(scale)
endif
if (scale lt -1) then begin
    iscale = fix(abs(scale))
    last_col = (ncols/iscale)*iscale - 1
    last_row = (nrows/iscale)*iscale - 1
    ncols = ncols / iscale
    nrows = nrows / iscale
endif

if (n_elements(start) eq 0) then start=0
if (n_elements(stop) eq 0) then stop=nframes
if (n_elements(step) eq 0) then step=1
if (n_elements(quality) eq 0) then quality=90
if (n_elements(order) eq 0) then order=0

if (n_elements(mpeg_file) ne 0) then begin
    if (!version.release lt '5.4') then begin
        mpegid = mpeg_open([ncols, nrows], file=mpeg_file)
    endif else begin
        mpegid = mpeg_open([ncols, nrows], file=mpeg_file, quality=quality)
    endelse
    mpeg_mode = 1
endif else begin
    mpeg_mode = 0
endelse

if (n_elements(jpeg_file) ne 0) then begin
    jpeg_mode = 1
endif else begin
    jpeg_mode = 0
endelse


for i=start, stop-1, step do begin
    case index of
        1: temp=vol[i,*,*]
        2: temp=vol[*,i,*]
        3: temp=vol[*,*,i]
    endcase
    temp = reform(temp, /overwrite)
    temp = bytscl(temp, min=min, max=max)
    if (scale ne 1) then temp = rebin(temp[0:last_col, 0:last_row], $
                                      ncols, nrows)
    ; The code for handling color was lifted out of the IDL procedure MPEG_PUT.
    if (keyword_set(color)) then begin
        ; Apply color tables and create an (ncols, 3 * nrows) array:
        tvlct, red, green, blue, /get
        temp = [[red[temp]], [green[temp]], [blue[temp]]]
        ; Reform to (ncols * nrows, 3) and then transpose to interleave color as 
        ; first dimension.
        temp = transpose(reform(temp, ncols * nrows, 3, /overwrite))
        ; Now back to (3, ncols, nrows)
        temp = reform(temp, 3, ncols, nrows, /overwrite)
    endif
    if (mpeg_mode) then begin
        print, 'Frame = ', i
        mpeg_put, mpegid, frame=i, image=temp, order=!order
    endif else if (jpeg_mode) then begin
        num = string(i+1, format='(i4.4)')
        file = jpeg_file + '_' + num + '.jpg'
        print, 'Writing jpeg file ', file
        if (keyword_set(color)) then true=1 else true=0
        write_jpeg, file, temp, quality=quality, order=!order, true=true
    endif else begin
        tv, temp
        if (keyword_set(label)) then begin
            ypos = n_elements(temp(0,*)) - 15
            xyouts, 0, ypos, strtrim(i,2), /device
        endif
        wait, wait
    endelse
endfor

if (mpeg_mode) then begin
    mpeg_save, mpegid, file=file
    mpeg_close, mpegid
endif

end
