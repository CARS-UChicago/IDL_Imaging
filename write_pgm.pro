pro write_pgm, file, array
;+
; NAME:
;   WRITE_PGM
;
; PURPOSE:
;   This procedures writes a portable greymap file.  Most Unix systems explain the PGM
;   standard under "man pgm".
;
; CATEGORY:
;   Imaging
;
; CALLING SEQUENCE:
;
;   WRITE_PGM, File, Array
;
; INPUTS:
;   File:   The name of the output file.
;
;   Array:  The array to write to the file.  This should be a 2-D array, byte, int, 
;           or long, signed or unsigned
;
; OUTPUTS:
;   None
;
; EXAMPLE:
;   data = indgen(100, 100)
;   WRITE_PGM, 'test.pgm', data
;
; MODIFICATION HISTORY:
;   Written by: Mark Rivers, Jan. 8, 2001
;   22-Jan-2001  MLR  Fixed bug which was causing maximum value not to be written to 
;                     the file
;-

    openw, lun, /get, file
    printf, lun, 'P2'
    s = size(array)
    if (s[0] ne 2) then message, 'Array must be 2-D'
    nx = s[1]
    ny = s[2]
    printf, lun, '# FILE=', file, ', NX=', nx, ', NY=', ny
    printf, lun, nx, ny
    max = max(array)
    printf, lun, '# Max= ', max
    printf, lun, max
    printf, lun, '# Data:'
    for j=0, ny-1 do begin
        for i=0, nx-1 do begin
            printf, lun, strtrim(array[i,j],2)
        endfor
    endfor
    free_lun, lun
end
