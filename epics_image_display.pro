pro epics_image_display, image_pv, _EXTRA=extra

;+
; NAME:
;       EPICS_IMAGE_DISPLAY
;
; PURPOSE:
;       This procedure is a general purpose routine to display EPICS arrays as
;       images.
;       It uses IMAGE_DISPLAY to perform interactive pan, zoom and scroll, live update of row and
;       column profiles, etc.
;
; CATEGORY:
;       Imaging
;
; CALLING SEQUENCE:
;       EPICS_IMAGE_DISPLAY, ImagePV
;
; INPUTS:
;       ImagePV:   The name of an EPICS PV containing 2-D array data to be displayed.
;
; KEYWORD PARAMETERS:
;       This procedure simply passes all keywords to <A HREF=#EPICS_IMAGE_DISPLAY::INIT>EPICS_IMAGE_DISPLAY::INIT</A>
;       by the keyword inheritence mechanism (_EXTRA).  See the documentation
;       for EPICS_IMAGE_DISPLAY::INIT for more information.
;
; PROCEDURE:
;       This procedure simply create a new object of class EPICS_IMAGE_DISPLAY.
;       See the documentation for IMAGE_DISPLAY for more information.
;
; PREBUILT VERSION:
;       In addition to the source code version of these files, the file epics_image_display.sav
;       is included in the distribution tar file.  This file can be run for free 
;       (no IDL license needed) with the IDL Virtual Machine.
;
; IMPORTANT NOTES:
;       The environment variable EZCA_IDL_SHARE must be set to point to the complete path
;       to the shareable library ezcaIDL.so (Linux, Unix, and Mac) or ezcaIDL.dll (Windows).
;       Note that the ezcaIDL shareable library or DLL must be built with EPICS R3.14 or later
;       in order to use arrays larger than 16000 bytes.
;
;       The environment variable EPICS_CA_MAX_ARRAY_BYTES must be set to at least the number
;       of bytes in the the image data array.
;
; EXAMPLE:
;       IDL> epics_image_display, 'GSE-PILATUS1:ImageData', nx=487, ny=195
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers (21-Jul-2007)
;-

    t = obj_new('epics_image_display', image_pv, _EXTRA=extra)

end
