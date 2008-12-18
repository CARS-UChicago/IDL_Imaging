function read_nd_netcdf, file, uniqueId, timeStamp, dimInfo

;+
; NAME:
;   READ_ND_NETCDF
;
; PURPOSE:
;   This procedures reads a simple NDFile netCDF file written by the EPICS area_detector module
;
; CATEGORY:
;   Detectors.
;
; CALLING SEQUENCE:
;   data = READ_ND_NETCDF(file)
;
; INPUTS:
;   File:
;       The name of the input file.
;
; OUTPUTS:
;       The N-D array of data.
;
; EXAMPLE:
;   data = READ_ND_NETCDF('test_001.nc')
;
; MODIFICATION HISTORY:
;   Written by:     Mark Rivers, April 17, 2008
;-

    if (n_elements(file) eq 0) then file=dialog_pickfile(/must_exist)
    ncdf_control, 0, /noverbose
    file_id = ncdf_open(file, /nowrite)
    if (n_elements(file_id) eq 0) then begin
        ; This is not a netCDF file
        message, 'File is not netCDF file'
    endif
    ; This is a netCDF file
    ; Process the global attributes
    dimInfo = {netCDFDimInfo, $
               size:    0L, $
               offset:  0L, $
               binning: 0L, $
               reverse:  0L}
    ncdf_attget, file_id, /global, 'numArrayDims', ndims
    dimInfo = replicate(dimInfo, ndims)
    ncdf_attget, file_id, /global, "dimSize", size
    ncdf_attget, file_id, /global, "dimOffset", offset
    ncdf_attget, file_id, /global, "dimBinning", binning
    ncdf_attget, file_id, /global, "dimReverse", reverse
    for i=0, ndims-1 do begin
        dimInfo[i].size   = size[i]
        dimInfo[i].offset = offset[i]
        dimInfo[i].binning = binning[i]
        dimInfo[i].reverse = reverse[i]
    endfor
    ; Get the data variable ids
    array_data_id   = ncdf_varid (file_id, 'array_data')
    uniqueId_id   = ncdf_varid (file_id, 'uniqueId')
    timeStamp_id   = ncdf_varid (file_id, 'timeStamp')

    if (array_data_id eq -1) then begin
        ncdf_close, file_id
        message, 'No array_data variable in netCDF file'
    endif

    ; Get information about the volume variable
    data_info = ncdf_varinq(file_id, array_data_id)

    ; Read the entire array for now
    ncdf_varget, file_id, array_data_id, data

    ; Read the entire array for now
    ncdf_varget, file_id, uniqueId_id, uniqueId

    ; Read the entire array for now
    ncdf_varget, file_id, timeStamp_id, timeStamp

    ; Close the netCDF file
    ncdf_close, file_id

    return, data
end
