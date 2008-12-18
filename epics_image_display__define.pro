pro epics_image_display::connect_image_pv
    status = caSetMonitor(self.image_pv)
    if (status ne 0) then begin
        t = dialog_message('Unable to connect to image PV: ' + self.image_pv)
        self.image_pv_valid = 0;
        widget_control, self.widgets.message, set_value='Unable to connect to image PV: ' + self.image_pv
    endif else begin
        self.image_pv_valid = 1
        widget_control, self.widgets.message, set_value='Connected to image PV: ' + self.image_pv
        self->display_image, retain=0
    endelse
end


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

    catch, err
    if (err ne 0) then begin
       t = dialog_message(!error_state.msg, /error)
        widget_control, self.widgets.status, set_value=!error_state.msg
        goto, end_event
    endif

    case event.id of

        self.widgets.exit: begin
            widget_control, event.top, /destroy
            obj_destroy, self
            return
        end

        self.widgets.nx: begin
            widget_control, self.widgets.nx, get_value=t
            self.nx = t
        end

        self.widgets.ny: begin
            widget_control, self.widgets.ny, get_value=t
            self.ny = t
        end

        self.widgets.image_pv: begin
            widget_control, self.widgets.image_pv, get_value=t
            if (strlen(t) gt 0) then begin
                self.image_pv = t
                self->connect_image_pv
            endif
        end

        self.widgets.timer: begin
            ; Timer event to check for new data and display it.
            if (self.image_pv_valid) then begin
                t = caCheckMonitor(self.image_pv)
                if (t eq 1) then begin
                    ; There is new data, display it
                    self->display_image, /retain
                endif
            endif
            widget_control, self.widgets.timer, timer=self.timer_interval
        end

        else:  t = dialog_message('Unknown event')
    endcase

    end_event:
end

pro epics_image_display::display_image, retain=retain
    widget_control, self.widgets.message, set_value='Got new image at ' + systime(0)
    t = caget(self.image_pv, data, max=self.nx*self.ny)
    if (t ne 0) then begin
        print, 'Error reading image'
        return
    endif
    if (n_elements(data) ne self.nx*self.ny) then begin
        print, 'Image is wrong size'
        return
    endif
    data = reform(data, self.nx, self.ny, /overwrite)
    ;iimage, data, overplot=self.iimage_id
    self.image_display->scale_image, data, /leave_mouse, retain=retain
end


function epics_image_display::init, image_pv, nx=nx, ny=ny, delay=delay

;+
; NAME:
;       EPICS_IMAGE_DISPLAY::INIT
;
; PURPOSE:
;       The EPICS_IMAGE_DISPLAY class provides a general purpose routine to display
;       EPICS arrays as images. It uses <A HREF=#IMAGE_DISPLAY>IMAGE_DISPLAY</A> to perform interactive pan,
;       zoom and scroll, live update of row and column profiles, etc.
;
;       This function initializes an object of class EPICS_IMAGE_DISPLAY.  It is
;       not called directly, but is called indirectly when a new object of
;       class EPICS_IMAGE_DISPLAY is created via OBJ_NEW('EPICS_IMAGE_DISPLAY')
;
;       The EPICS_IMAGE_DISPLAY object creates simple GUI display which provides
;       widgets to control the EPICS PV to display, the number of rows and columns in
;       the images, and a status display.
;
;       EPICS_IMAGE_DISPLAY waits new data for the EPICS ImagePV array, and then displays the
;       new image with the IMAGE_DISPLAY routine.
;
; CATEGORY:
;       Imaging
;
; CALLING SEQUENCE:
;       obj = OBJ_NEW('EPICS_IMAGE_DISPLAY', ImagePV)
;
; INPUTS:
;       ImagePV:   The name of an EPICS PV containing 2-D array data to be displayed.
;       EPICS only directly supports 1-D arrays at present.  2-D data can be stored in
;       EPICS arrays, but then the number of rows and columns must be independently
;       specified, since it is not provided with the array information.
;
;       In order to display images that contain more than 16000 bytes (which most images do!)
;       it is necessary that both the EPICS server (e.g. IOC) and the EPICS client
;       (i.e. ezcaIDL.so or ezcaIDL.dll) be built with EPICS 3.14 or later.
;       Furthermore, one must set the environment variable EPICS_CA_MAX_ARRAY_BYTES to be
;       at least the size of the image array data.  This must be done on both the client and
;       server machines.
;       For example, with the Pilatus detector we typically set EPICS_CA_MAX_ARRAY_BYTES=500000.
;
; KEYWORD PARAMETERS:
;       NX:
;           The number of columns in the ImagePV array.  Default=487, which is the value for
;           the Pilatus 100K detector.
;       NY:
;           The number of rows in the ImagePV array.  Default=195, which is the value for
;           the Pilatus 100K detector.
;       DELAY:
;           The time between polls to see if there is new data for ImagePV.
;           Default=.01 seconds.
;           The check for new data is done using a Channel Access monitor, so it is fast.
;
; OUTPUTS:
;       This function returns 1 to indicate that the object was successfully
;       created.
;
; REQUIREMENTS:
;       This function requires <A HREF=#IMAGE_DISPLAY>IMAGE_DISPLAY__DEFINE.PRO</A>,
;       GET_FONT_NAME.PRO, and <A HREF=ezcaIDLGuide.html>ezcaIDL</A>.
;       EZCA_IDL requires the shareable library (ezcaIDL.so or ezcaIDL.dll) that
;       contains the functions used to communicate with EPICS.
;
; EXAMPLE:
;       IDL> obj = OBJ_NEW('EPICS_IMAGE_DISPLAY', 'GSE-PILATUS1:ImageData', nx=487, ny=195)
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers (6-June-2007).
;-
    ; We assume Pilatus dimensions if they are not specified
    if (n_elements(nx) eq 0) then nx=487
    if (n_elements(ny) eq 0) then ny=195
    if (n_elements(delay) eq 0) then delay=0.01
    if (n_elements(image_pv) eq 0) then image_pv=''
    self.image_pv = image_pv
    self.nx = nx
    self.ny = ny
    self.image_pv_valid = 0

    data = lonarr(self.nx, self.ny)
    ;iimage, data, identifier=id
    ;self.iimage_id=id
    ; We make the default order be top to bottom, image_display can change it
    t = obj_new('image_display', data, order=1)
    self.image_display = t;

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
    row = widget_base(self.widgets.base, /row)
    self.widgets.image_pv = cw_field(row, /column, title='Image PV', xsize=30, /return_events)
    self.widgets.nx = cw_field(row, /column, /integer, title='NX', xsize=8, /return_events)
    self.widgets.ny = cw_field(row, /column, /integer, title='NY', xsize=8, /return_events)
    row = widget_base(self.widgets.base, /row)
    self.widgets.message = cw_field(row, /row, title='Message', xsize=50)
    widget_control, self.widgets.image_pv, set_value=self.image_pv
    widget_control, self.widgets.nx, set_value=self.nx
    widget_control, self.widgets.ny, set_value=self.ny

    widget_control, self.widgets.base, set_uvalue=self
    widget_control, self.widgets.base, /realize

    ; Timer widgets
    self.widgets.timer = self.widgets.base
    self.timer_interval = 0.01
    widget_control, self.widgets.timer, timer=self.timer_interval

    xmanager, 'epics_image_display', self.widgets.base, /no_block
    if (strlen(self.image_pv) gt 0) then self->connect_image_pv
    return, 1
end

pro epics_image_display::cleanup

end

pro epics_image_display__define

    widgets={ epics_image_display_widgets, $
        base:          0L, $
        filename:      0L, $
        image_pv:      0L, $
        nx:            0L, $
        ny:            0L, $
        message:       0L, $
        exit:          0L, $
        timer:         0L $
    }

    fonts = {epics_image_display_fonts, $
        normal: '', $
        heading1: '', $
        heading2: '' $
    }

    epics_image_display = {epics_image_display, $
        widgets:        widgets, $
        fonts:          fonts, $
        image_display:  obj_new(), $
        iimage_id:      "", $
        image_pv:       "", $
        image_pv_valid: 0L, $
        nx:             0L, $
        ny:             0L, $
        timer_interval: 0.0 $
    }
end
