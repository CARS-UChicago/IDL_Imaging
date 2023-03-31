pro image_display, data, _EXTRA=extra
;+
; NAME:
;   IMAGE_DISPLAY
;
; PURPOSE:
;   This procedure is a general purpose image display routine.
;   It allows interactive pan, zoom and scroll, live update of row and column profiles, etc.
;
; CALLING SEQUENCE:
;   IMAGE_DISPLAY, DATA, KEYWORD=VALUE
;
; INPUTS:
;   DATA:
;     A 2-D or 3-D array to be displayed
;
; KEYWORD PARAMETERS:
;   This procedure simply passes all keywords to IMAGE_DISPLAY::INIT by
;   the keyword inheritence mechanism (_EXTRA).  
;   See the documentation for IMAGE_DISPLAY::INIT for more information.
;
; PROCEDURE:
;   This procedure simply create a new object of class IMAGE_DISPLAY.
;   See the documentation for the IMAGE_DISPLAY class library for more information.
;-

  object = obj_new('image_display', data, _EXTRA=extra)
end
