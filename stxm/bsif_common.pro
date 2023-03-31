;-
; NAME:
;       BSIF_COMMON.PRO
; PURPOSE:
; Defines the common block used in our imaging routines for images read
; from BSIF files.
;   VERSION
;       BSIF file format version number - not currently used, allows for future
;       changes in file format.
;   FIRST_DATA_RECORD
;       First data record in file. This field is set by WRITE_BSIF and is not
;       needed by any IDL routines.
;   N_COLS, NROWS
;       The number of columns and rows in the image
;   N_DATA
;       The number of data values at each pixel
;   X_NORMAL
;       Non-zero (TRUE) if X scan direction is "normal" e.g. left-to-right.
;       This variable is not presently used by any IDL routines.
;   Y_NORMAL
;       Non-zero (TRUE) if Y scan direction is "normal" e.g. top-to-bottom.
;       This variable is not presently used by any IDL routines.
;   ROTATED
;       Non-zero (TRUE) image image is rotated, e.g. X and Y axes are switched.
;       This variable is not presently used by any IDL routines.
;   X_START, X_STOP
;       The position of the beginning and end of the X scan direction in user 
;       units, such as mm.
;   Y_START, Y_STOP
;       The position of the beginning and end of the Y scan direction in user 
;       units, such as mm.
;   X_DIST(N_COLS)
;       Array with calibrated X axis positions.
;   Y_DIST(N_ROWS)
;       Array with calibrated Y axis positions.
;   DATA_TYPE
;       The data type of image_data as stored in the file.
;   COMPRESSION_TYPE
;       Data compression type. Currently supported values are
;           0 = No compression
;           1 = ???
;           2 = ???
;   DATA_MIN(N_DATA), DATA_MAX(N_DATA)
;       The minimum and maximum values of the data in the array. These values
;       are not guaranteed to be correct.
;   IMAGE_TITLE
;       The title of the image - a character string
;   X_TITLE
;       The title of the X axis of the image, i.e. "Distance in mm"
;   Y_TITLE
;       The title of the Y axis of the image, i.e. "Distance in mm"
;   DATA_TITLE(N_DATA)
;       The titles of the data values at each pixel.
;   USER_BUFFER
;       A user defined array. Stored in IDL as a byte array. May have internal
;       structure which is user defined.
;   IMAGE_DATA(COLS, ROWS, N_DATA)
;       The image data array. The data type of the array is determined by the 
;       data type definined in the BSIF file. It can be BYTE, INTEGER, LONG or 
;       FLOAT.
;-
common BSIF, $
    version,            $       ; BSIF file format version number
    first_data_record,  $       ; First data record in file
    n_rows, n_cols,     $       ; Number of rows and columns in image
    n_data,             $       ; Number of data values at each pixel
    x_normal, y_normal, $       ; Flags signaling top/bottom, left/right order
    rotated,            $       ; Flag signaling 90 degree rotation
    x_start, x_stop,    $       ; X range coordinates in user units
    y_start, y_stop,    $       ; Y range in user units
    x_dist, y_dist,     $       ; Arrays with calibrated units
    data_type,          $       ; Data type
    compression_type,   $       ; Data compression type
    data_min, data_max, $       ; Range of data
    image_title,        $       ; Title of image
    x_title,            $       ; Title of X axis
    y_title,            $       ; Title of Y axis
    data_title,         $       ; Titles of data values (array)
    user_buffer,        $       ; User defined buffer
    image_data                  ; The image data (n_cols, n_rows, n_data)
