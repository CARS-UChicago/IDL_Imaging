pro detect_2d_peaks_event, event
    widget_control, event.top, get_uvalue=uvalue
    widgets = uvalue.widgets
    widget_control, widgets.min_display, get_value=min_display
    widget_control, widgets.max_display, get_value=max_display

    case event.id of
        widgets.redisplay: begin
            data = *(uvalue.pdata)
            if (max_display le min_display) then max_display = max(data)
    		image_display, data, min=min_display, max=max_display
        end

    	widgets.read: begin
    		widget_control, /hourglass
    		file = dialog_pickfile(/read, /must_exist)
    		if (file eq '') then return
    		read_princeton, file, data
    		bin = widget_info(widgets.binning, /droplist_select)
    		bin = 2^bin  ; 0, 1, 2 = 1, 2, 4
    		nx = n_elements(data(*,0))
    		ny = n_elements(data(0,*))
    		ptr_free, uvalue.pdata
    		data = rebin(data, nx/bin, ny/bin)
            if (max_display le min_display) then max_display = max(data)
    		image_display, data, min=min_display, max=max_display
    		ptr_free, uvalue.pdata
    		uvalue.pdata = ptr_new(data)
    		widget_control, event.top, set_uvalue=uvalue
    	end

        widgets.compute: begin
        	widget_control, /hourglass
            widget_control, widgets.fwhm, get_value=fwhm
            widget_control, widgets.min_sharp, get_value=min_sharp
            widget_control, widgets.max_sharp, get_value=max_sharp
            widget_control, widgets.min_round, get_value=min_round
            widget_control, widgets.max_round, get_value=max_round
            widget_control, widgets.threshold, get_value=threshold
            data = *(uvalue.pdata)
       		nx = n_elements(data(*,0))
    		ny = n_elements(data(0,*))
            find_2d_peaks, data, x, y, flux, sharp, roundness, threshold, fwhm, $
                           [min_round, max_round], [min_sharp, max_sharp], /silent
            if (max_display le min_display) then max_display = max(data)
            b = bytscl(data, min=min_display, max=max_display, top=253)
            tvlct, 255, 0, 0, 254  ; Load red into next to top color table entry
            for i=0, n_elements(x)-1 do begin
                ix = round(x[i])
                iy = round(y[i])
                x0 = ix-1 > 0
                x1 = ix+1 < nx-1
                y0 = iy-1 > 0
                y1 = iy+1 < ny-1
                b[x0:x1, y0:y1] = 254
            endfor
            image_display, b, min=0, max=255
        end

        widgets.exit:  widget_control, event.top, /destroy
        else:

    endcase
end



pro detect_2d_peaks

    widgets = $
        {base: 0L, $
         read: 0L, $
         exit: 0L, $
         binning: 0L, $
         FWHM: 0L, $
         min_sharp: 0L, $
         max_sharp: 0L, $
         min_round: 0L, $
         max_round: 0L, $
         threshold: 0L, $
         min_display: 0L, $
         max_display: 0L, $
         redisplay: 0L, $
         compute: 0L $
        }
    widgets.base = widget_base(/column, mbar=mbar)
    row = widget_base(widgets.base, /row)
    file = widget_button(mbar, /menu, value='File')
    widgets.read = widget_button(file, value='Read ...')
    widgets.exit = widget_button(file, value='Exit')
    col = widget_base(row, /column)
    t = widget_label(col, value='Binning')
    widgets.binning = widget_droplist(col, value=['1', '2', '4'])
    widgets.FWHM = cw_field(row, /column, title='FWHM', value=3, /long, xsize=6)
    widgets.min_sharp = cw_field(row, /column, title='Min. sharpness', value=.2, /float, xsize=10)
    widgets.max_sharp = cw_field(row, /column, title='Max. sharpness', value=1., /float, xsize=10)
    widgets.min_round = cw_field(row, /column, title='Min. roundness', value=-1., /float, xsize=10)
    widgets.max_round = cw_field(row, /column, title='Max. roundness', value=1., /float, xsize=10)
    widgets.threshold = cw_field(row, /column, title='Peak threshold', value=1000., /float, xsize=10)
    widgets.min_display = cw_field(row, /column, title='Min. display', value=0, /long, xsize=10)
    widgets.max_display = cw_field(row, /column, title='Max. display', value=0, /long, xsize=10)
    row = widget_base(widgets.base, /row)
    widgets.redisplay = widget_button(row, value='Re-display data')
    widgets.compute = widget_button(row, value='Find peaks')
    widget_control, widgets.base, /realize
    uvalue = {widgets: widgets, pdata: ptr_new()}
    widget_control, widgets.base, set_uvalue=uvalue
    xmanager, 'detect_2d_peaks', widgets.base, /no_block
end