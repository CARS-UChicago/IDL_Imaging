pro epics_image_display_event, event
    widget_control, event.top, get_uvalue=epics_image_display
    epics_image_display->event, event
end


pro epics_image_display::event, event
    if (tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST') then begin
        widget_control, event.top, /destroy
        obj_destroy, self
        return
    endif

    ;catch, err
    ;if (err ne 0) then begin
    ;   t = dialog_message(!error_state.msg, /error)
    ;    widget_control, self.widgets.status, set_value=!error_state.msg
    ;    goto, end_event
    ;endif

    case event.id of

        self.widgets.exit: begin
            widget_control, event.top, /destroy
            obj_destroy, self
            return
        end

        self.widgets.display_timer: begin
            ; Timer event to check for new data and display it.
            t = caCheckMonitor(self.image_pv)
            if (t eq 1) then begin
                ; There is new data, display it
                self->display_image
            endif
            widget_control, self.widgets.display_timer, timer=self.display_timer_interval
        end

        else:  t = dialog_message('Unknown event')
    endcase

    end_event:
end

pro epics_image_display::display_image
    print, 'Got new image at ', systime(0)
    t = caget(self.image_pv, data)
    if (t ne 0) then begin
        print, 'Error reading image'
        return
    endif
    if (n_elements(data) ne self.nx*self.ny) then begin
        print, 'Image is wrong size'
        return
    endif
    data = reform(data, self.nx, self.ny, /overwrite)
    ;iimage, data, overplot=self.iimage_id\
    self.image_display->scale_image, data, /leave_mouse
end

function epics_image_display::init, image_pv, nx=nx, ny=ny, delay=delay

;+
; NAME:
;       epics_image_display::init
;
; Displays EPICS arrays as images.
; Remember to set the environment variable EPICS_CA_MAX_ARRAY_BYTES to a
; value at least as large as your images
; You must also use the ezcaIDL shareable library built with EPICS 3.14, not 3.13.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers (6-June-2007).
;-
    ; We assume Pilatus dimensions if they are not specified
    if (n_elements(nx) eq 0) then nx=487
    if (n_elements(ny) eq 0) then ny=195
    if (n_elements(delay) eq 0) then delay=0.01

    if (n_elements(image_pv) eq 0) then error, 'Must specify PV name'
    self.image_pv = image_pv
    self.nx = nx
    self.ny = ny

    t = caSetMonitor(self.image_pv)
    data = lonarr(self.nx, self.ny)
    ;iimage, data, identifier=id
    ;self.iimage_id=id
    t = obj_new('image_display', data)
    self.image_display = t;
    self->display_image

    self.fonts.normal = get_font_name(/helvetica)
    self.fonts.heading1 = get_font_name(/large, /bold)
    self.fonts.heading2 = get_font_name(/bold)

    cainit  ; Need to call this in case it was not called in startup script, i.e. Virtual Machine
    casettimeout, .01
    casetretrycount, 100
    self.widgets.base= widget_base(column=1, /tlb_kill_request_events, $
                                   title='EPICS Image Display', $
                                   mbar=mbar, tab_mode=1)

    ; File menu
    file = widget_button(mbar, /menu, value = 'File')
    self.widgets.exit = widget_button(file, $
                                            value = 'Exit')

    widget_control, self.widgets.base, set_uvalue=self
    widget_control, self.widgets.base, /realize

    ; Timer widgets
    self.widgets.display_timer = self.widgets.base
    self.display_timer_interval = 0.1
    widget_control, self.widgets.display_timer, timer=self.display_timer_interval
    xmanager, 'epics_image_display', self.widgets.base, /no_block
    return, 1
end

pro epics_image_display::cleanup

end

pro epics_image_display__define

    widgets={ epics_image_display_widgets, $
        base: 0L, $
        filename: 0L, $
        exit: 0L, $
        display_timer: 0L $
    }

    fonts = {epics_image_display_fonts, $
        normal: '', $
        heading1: '', $
        heading2: '' $
    }

    epics_image_display = {epics_image_display, $
        widgets: widgets, $
        fonts: fonts, $
        image_display: obj_new(), $
        iimage_id: "", $
        image_pv: "", $
        nx: 0L, $
        ny: 0L, $
        display_timer_interval: 0.0 $
    }
end
