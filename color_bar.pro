pro color_bar, xstart, ystart, length=length, width=width, $
               horizontal=horizontal, vertical=vertical, normal=normal

;+
; NAME:
;       COLOR_BAR.PRO
; PURPOSE:
;       To output a color bar on an image display.
; CALLING SEQUENCE:
;       COLOR_BAR, x_start, y_start, [keywords]
; OPTIONAL INPUT PARAMETERS:
;       If the following input parameters are omitted the routine will prompt
;       the user to position the mouse and click at the desired location of the
;       color bar.
;   X_START
;       The X coordinate of the lower left corner of the color bar.
;   Y_START
;       The Y coordinate of the lower left corner of the color bar.
; KEYWORD PARAMETERS:
;   LENGTH
;       The length of the color bar. The default is 256 pixels in device
;       coordinates.
;   WIDTH
;       The width of the color bar. The default is 20 pixels in device
;       coordinates.
;   /HORIZONTAL
;       Draws the color bar horizontally. The default is to draw it vertically.
;   /VERTICAL
;       Draws the color bar vertically. This is the default.
;   /DEVICE
;       Specifies that X_START, Y_START, LENGTH and WIDTH are in device
;       coordinates (pixels). This is the default
;   /NORMAL
;       Specifies that X_START, Y_START, LENGTH and WIDTH are in normalized
;       coordinates (0 to 1 on each axis)
; OUTPUTS:
;       None
; COMMON BLOCKS:
;       None
; SIDE EFFECTS:
;       Draws a color bar on the current output device.
; RESTRICTIONS:
;       The addition of the /NORMAL keyword makes this routine work correctly
;       devices with different pixel resolutions, such as high-resolution
;       windows and the Digital Palette. It still doesn't work on PostScript,
;       because of the scalable pixels problem.
; PROCEDURE:
;       Draws a color bar which is WIDTH by LENGTH.
;       The color bar goes from color 0 at the bottom (or left) edge to the
;       maximum color (!D.TABLE_SIZE-1) at the top (or right) edge. The color
;       bar is surrounded by two borders, one in color 0 and one in the maximum
;       color. These borders are one pixel wide. The color bar is drawn on the
;       current graphics device using routine TV.
; MODIFICATION HISTORY:
;       Created 1990 by Mark Rivers.
;       Modified November 1991 to change the DIRECTION keyword to /HORIZONTAL
;       and /VERTICAL since these are easier to remember.
;       Modified November 1991 to add support for NORMAL coordinates. This
;       makes the routine much more device independent.
;-
num_colors = !D.TABLE_SIZE
if n_params() lt 2 then begin
  print, "Move cursor to position color bar, press button when ready"
  cursor, xstart, ystart, /normal, /down
  print, 'X, Y = ', xstart, ystart, ' in normalized coordinates'
  normal = 1
endif
if keyword_set(normal) then begin
  if n_elements(width) eq 0 then width = .05
  if n_elements(length) eq 0 then length = .5
  xstart = xstart * !d.x_vsize
  width = width * !d.x_vsize
  ystart = ystart * !d.y_vsize
  length = length * !d.y_vsize
endif else begin
  if n_elements(width) eq 0 then width = 20
  if n_elements(length) eq 0 then length = 256
endelse

if keyword_set(horizontal) then horiz=1 else horiz=0

bar = bytarr(length, width)
ramp = findgen(length-4) * (num_colors-1) / (length-5)

; Make outline of color bar in both color 0 and color num_colors-1
bar(0:length-1, 0) = 0
bar(1:length-2, 1) = num_colors-1
bar(1:length-2, width-2) = num_colors-1
bar(0:length-1, width-1) = 0
bar(0, 0:width-1) = 0
bar(1, 1:width-2) = num_colors-1
bar(length-2, 1:width-2) = num_colors-1
bar(length-1, 0:width-1) = 0
for i=2, width-3 do begin
  bar(2, i) = byte(ramp)
endfor
if (not horiz) then bar = transpose(bar)
; Draw bottom to top, save current value of !order
old_order = !order
!order=0
tv, bar, xstart, ystart
!order = old_order
return
end
