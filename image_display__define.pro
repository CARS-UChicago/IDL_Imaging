pro image_display::write_output_file, outfile=outfile, output_width=output_width
  if (n_elements(outfile) eq 0) then begin
    ; Replace multiple spaces with single space
    file = strcompress(self.image_data.title)
    ; Replace space with _
    file = file.replace(' ', '_')
    filt = ['*.bmp', '*.jpg', '*.jpeg', '*.pdf', '*.png', '*.tif', '*.tiff']
    outfile = dialog_pickfile(filter=filt, file=file+'.jpg', /write, title='Set output file (extension determines file format)')
  endif
  if (n_elements(output_width) eq 0) then output_width = 2048
  self.image_window.image_obj->save, outfile, width=output_width
  return
end
;
;------------------
pro image_display::plot_profiles, x_mouse, y_mouse

  if (n_elements(x_mouse) eq 0) then x_mouse = self.image_window.x_size/2
  if (n_elements(y_mouse) eq 0) then y_mouse = self.image_window.y_size/2
  coords = self.image_window.image_obj->convertcoord(x_mouse, y_mouse, /device, /to_data)
  xc = coords[0]
  yc = coords[1]
  if (self.image_window.order EQ 1) then yc = self.image_data.y_size - yc - 1
  xc = xc > 0 < (self.image_data.x_size-1)
  yc = yc > 0 < (self.image_data.y_size-1)
  coords = self.image_window.image_obj->convertcoord(x_mouse, y_mouse, /device, /to_normalized)
  xnorm = coords[0]
  ynorm = coords[1]
  
  widget_control, self.widgets.x_pixel, set_value = string(xc, format='(i)')
  widget_control, self.widgets.y_pixel, set_value = string(yc, format='(i)')
  widget_control, self.widgets.x_user, set_value = string((*self.image_data.x_dist)[xc])
  widget_control, self.widgets.y_user, set_value = string((*self.image_data.y_dist)[yc])
  widget_control, self.widgets.pixel_value, set_value = string((*self.image_data.raw_data)[xc, yc])

  x_axis = *self.image_data.x_dist
  y_axis = *self.image_data.y_dist
  x_min = (self.image_window.image_obj).xrange[0] > 0 < (self.image_data.x_size-1)
  x_max = (self.image_window.image_obj).xrange[1] > 0 < (self.image_data.x_size-1)
  y_min = (self.image_window.image_obj).yrange[0] > 0 < (self.image_data.y_size-1)
  y_max = (self.image_window.image_obj).yrange[1] > 0 < (self.image_data.y_size-1)
  black_level = (self.image_window.image_obj).min_value
  white_level = (self.image_window.image_obj).max_value

  plot = self.row_plot_window.plot_obj
  x = *self.image_data.x_dist
  y = reform((*self.image_data.raw_data)[*, yc])
  plot.setdata, x, y
  plot.xrange = [x_axis[x_min], x_axis[x_max]]
  plot.yrange = [black_level, white_level]
  arrow = self.row_plot_window.arrow_obj
  arrow.position = ([xnorm, 0, xnorm, 1])
  
  plot = self.col_plot_window.plot_obj
  x = reform((*self.image_data.raw_data)[xc, *])
  y = *self.image_data.y_dist
  plot.setdata, x, y
  plot.xrange = [black_level, white_level]
  plot.yrange = [y_axis[y_min], y_axis[y_max]]
  arrow = self.col_plot_window.arrow_obj
  arrow.position = ([0, ynorm, 1, ynorm])

end


pro image_display::update_image

  ; Set the min/max intensity
  (self.image_window.image_obj).min_value = self.image_window.black_level
  (self.image_window.image_obj).max_value = self.image_window.white_level

  ; Set the color table 
  tvlct, table, /get
  (self.image_window.image_obj).rgb_table = table
  
  ; Set interpolation
  (self.image_window.image_obj).interpolate = self.image_window.interpolate

end


pro image_display::set_image_data, data, title=title

  if (n_elements(title) eq 0) then title = 'IDL Image Display'
  self.image_data.title = title
  widget_control, self.widgets.base, tlb_set_title=title

  ptr_free, self.image_data.raw_data
  self.image_data.raw_data = ptr_new(data)
  self.image_window.image_obj->setdata, data, order=self.image_window.order
  self->update_image
  self->plot_profiles
end

pro image_display::new_image, data, min=min, max=max, $
            interpolate=interpolate, order=order, $
            xdist=xdist, ydist=ydist, $
            title=title, leave_mouse=leave_mouse

;+
; NAME:
;       image_display::SCALE_IMAGE
;
; PURPOSE:
;       This routine displays a new image in an existing image_display object.
;
; CATEGORY:
;       Imaging
;
; CALLING SEQUENCE:
;       image_data->SCALE_IMAGE, Data
;
; INPUTS:
;       Data:   A 2-D array to be displayed
;
; KEYWORD PARAMETERS:
;       XDIST:  An array containing the user units ("distance") of the
;               X axis pixels.  Dimensions must be same as xsize of Data.
;
;       YDIST:  An array containing the user units ("distance") of the
;               Y axis pixels.  Dimensions must be same as ysize of Data.
;
;       MIN:    The minimum display intensity.  Default=min(Data).
;
;       MAX:    The maximum display intensity.  Default=max(Data).
;
;       ZOOM:   A scaler or 2-element (X,Y) array of integer zoom factors.
;               Default = 1 in each direction.  ZOOM=2 will zoom 2X in both
;               directions, ZOOM=[1,2] will zoom 1X in X, 2X in Y.
;
;       CENTER: The location where the center of the image should be located
;               in the display window.
;               The default is the center of the display window.
;               CENTER=[200,300] will center the image at X=200, Y=300
;
;       NOERASE: Set this flag to not erase the window before displaying the
;               image.  Allows multiple images to share a window.
;
;       INTERPOLATE:  Zoom the image by interpolation rather than replication.
;
;       REPLICATE: Zoom the image by replication rather than interpolation.
;
;       TITLE:  The title to give the display window.
;
;       SUBTITLE:  The subtitle to give the display window.
;
;       ORDER:  The order in which to display the image.
;               0=bottom to top
;               1=top to bottom
;               Default = Existing order
;
;       LEAVE_MOUSE:  Set this keyword to not move the mouse to the center of the
;               new image display.
;
;       RETAIN: Set this keyword to not reset the zoom and intensity scaling when
;               when the new data are displayed.  This requires that the dimensions
;               of the image be the same as the image currently displayed.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers (3-DEC-1998)
;       20-Nov-2007  MLR  Added ORDER and RETAIN keywords.
;
; EXAMPLE:
;       IDL> a = DIST(512)
;       IDL> obj = OBJ_NEW('image_display', a)
;       IDL> obj->SCALE_IMAGE, a+100, /RETAIN
;
;-


  ndims = size(data, /n_dimensions)
  if (ndims ne 2) then data = reform(data)
  dims = size(data, /dimensions)
  self.image_data.x_size = dims[0]
  self.image_data.y_size = dims[1]

  if (n_elements(order) ne 0) then begin
    self.image_window.order = order
    widget_control, self.widgets.order, set_droplist_select=self.image_window.order
  endif

  if (n_elements(xdist) eq 0) then xdist = findgen(self.image_data.x_size)
  if (n_elements(ydist) eq 0) then ydist = findgen(self.image_data.y_size)

  if (n_elements(xdist) ne self.image_data.x_size) or $
     (n_elements(ydist) ne self.image_data.y_size) then $
    t = dialog_message('Size of xdist and ydist must match size of image')
  ptr_free, self.image_data.x_dist
  ptr_free, self.image_data.y_dist
  self.image_data.x_dist = ptr_new(xdist)
  self.image_data.y_dist = ptr_new(ydist)

  if (n_elements(min) ne 0) then begin
    self.image_window.black_level = min
  endif else begin
    if (not keyword_set(retain)) then begin
      self.image_window.black_level = min(data)
    endif
  endelse

  if (n_elements(max) ne 0) then begin
    self.image_window.white_level = max
  endif else begin
    if (not keyword_set(retain)) then begin
      self.image_window.white_level = max(data)
    endif
  endelse

  if (n_elements(interpolate) ne 0) then self.image_window.interpolate=interpolate

  widget_control, self.widgets.min_actual, set_value = string(min(data))
  widget_control, self.widgets.max_actual, set_value = string(max(data))
  widget_control, self.widgets.min_set, set_value = string(min(data))
  widget_control, self.widgets.max_set, set_value = string(max(data))

  if (obj_valid(self.image_window.image_obj)) then (self.image_window.image_obj).erase
  (self.image_window.window_obj).select
  self.image_window.image_obj = image(intarr(2,2), /current, order=self.image_window.order, margin=0)

  xc = self.image_data.x_size/2
  yc = self.image_data.y_size/2
  if (obj_valid(self.row_plot_window.plot_obj)) then (self.row_plot_window.plot_obj).erase
  x = *self.image_data.x_dist
  y = reform(data[*, yc])
  (self.row_plot_window.window_obj).select
  self.row_plot_window.plot_obj = plot(x, y, /current, color='blue', margin=0)
  self.row_plot_window.arrow_obj = arrow([.5, .5], [0, 1], /norm, color='red', head_size=0)
  axes = (self.row_plot_window.plot_obj).axes
  axes[0].textpos = 1
  axes[1].textpos = 1

  if (obj_valid(self.col_plot_window.plot_obj)) then (self.col_plot_window.plot_obj).erase
  x = reform(data[xc, *])
  y = *self.image_data.y_dist
  (self.col_plot_window.window_obj).select
  self.col_plot_window.plot_obj = plot(x, y, /current, color='blue', margin=0)
  self.col_plot_window.arrow_obj = arrow([0, 1], [.5, .5], /norm, color='red', head_size=0)
  axes = (self.col_plot_window.plot_obj).axes
  axes[0].textpos = 1
  axes[0].text_orientation = 90
  axes[1].textpos = 1

  self->set_image_data, data, title=title
end


pro image_display_event, event
    widget_control, event.top, get_uvalue=image_display
    image_display->event, event
end

pro image_display::event, event
  if (tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST') then begin
    widget_control, event.top, /destroy
    obj_destroy, self
    return
  endif
  case event.id of
    self.widgets.image: begin
      case event.type of
        2: begin   ; Motion event
          x_mouse=event.x
          y_mouse=event.y
          self->plot_profiles, x_mouse, y_mouse
        end
        else:
      endcase
    end

    self.widgets.min_set: begin
      widget_control, event.id, get_value=value
      self.image_window.black_level = value
      self->update_image
    end

    self.widgets.max_set: begin
      widget_control, event.id, get_value=value
      self.image_window.white_level = value
      self->update_image
    end
    self.widgets.order: begin
      self.image_window.order = widget_info(event.id, /droplist_select)
      data = *self.image_data.raw_data
      self->set_image_data, data
    end

    self.widgets.zoom_mode: begin
      self.image_window.interpolate = widget_info(event.id, /droplist_select)
      self->update_image
    end
    
    self.widgets.reset_zoom: begin
      (self.image_window.image_obj).scale_center = [self.image_data.x_size/2, self.image_data.y_size/2]
      (self.image_window.image_obj).xrange = [0, self.image_data.x_size]
      (self.image_window.image_obj).yrange = [0, self.image_data.y_size]
    end

    self.widgets.autoscale: begin
      self.image_window.black_level=min(*self.image_data.raw_data)
      self.image_window.white_level=max(*self.image_data.raw_data)
      widget_control, self.widgets.min_set, set_value = string(self.image_window.black_level)
      widget_control, self.widgets.max_set, set_value = string(self.image_window.white_level)
      self->update_image
    end

    self.widgets.new_color_table: begin
      xloadct, /block
      self->update_image
    end

    self.widgets.write_output_file: begin
      widget_control, self.widgets.output_width, get_value=width
      self->write_output_file, output_width=width
    end


    else: t = dialog_message('Unknown event')
  endcase
end


function image_display::init, data, xsize=xsize, ysize=ysize, $
                                    xdist=xdist, ydist=ydist, min=min, max=max, $
                                    title=title, order=order
;+
; NAME:
;       image_display::INIT
;
; PURPOSE:
;       This function initializes an object of class image_display.  It is
;       not called directly, but is called indirectly when a new object of
;       class image_display is created via OBJ_NEW('image_display')
;
;       The image_display object is a GUI display which provides interactive
;       pan, zoom and scroll, live update of row and column profiles, etc.
;
; CATEGORY:
;       Imaging
;
; CALLING SEQUENCE:
;       obj = OBJ_NEW('image_display', Data)
;
; INPUTS:
;       Data:   A 2-D array to be displayed
;
; KEYWORD PARAMETERS:
;       XSIZE:  The number of pixels horizontally in the image window.
;               Default is the greater of 400 pixels or the xsize of Data.
;
;       YSIZE:  The number of pixels vertically in the image window.
;               Default is the greater of 400 pixels or the ysize of Data.
;
;       XDIST:  An array containing the user units ("distance") of the
;               X axis pixels.  Dimensions must be same as xsize of Data.
;
;       YDIST:  An array containing the user units ("distance") of the
;               Y axis pixels.  Dimensions must be same as ysize of Data.
;
;       MIN:    The minimum display intensity.  Default=min(Data).
;
;       MAX:    The maximum display intensity.  Default=max(Data).
;
;       TITLE:  The title to give the display window.
;
;       SUBTITLE:  The subtitle to give the display window.
;
;       ORDER:  The order in which to display the image.
;               0=bottom to top
;               1=top to bottom
;               Default = !ORDER system variable when this function is called.
;
; OUTPUTS:
;       This function returns 1 to indicate that the object was successfully
;       created.
;
; EXAMPLE:
;       IDL> a = DIST(512)
;       IDL> obj = OBJ_NEW('image_display', a)
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers (3-DEC-1998)
;       3-APR-1999  MLR  Fixed so that it will reform input array if it is
;                        greater than 2-D but with some dimensions=1,
;                        e.g. Data = intarr(100,1,100), which happens
;                        frequently when extracting slices from 3-D data
;       23-SEP-1999 MN   Added code to write JPEG file
;       12-APR-2001 MLR  Added cleanup code so that the image_display object and all
;                        pointers are free when the user closes the window.
;       11-JUN-2001 MN   Added dropdown menu for output types (JPEG, BMP, PNG), and
;                        added 'title' and 'subtitle' keywords which are
;                        displayed in the window title
;       17-JAN-2001 MLR  Delete line that called "a2f".  Line did nothing and a2f
;                        is not needed.
;       27-APR-2002 MLR  Added order keywords to write_jpeg and write_png so images
;                        are correct side up.
;                        Fixed bug in plotting vertical column profile if self.image_window.order=1, thanks
;                        to Peter Vontobel of Swiss Light Source.
;
;       4-JUN-2002  MLR  Added "title" keyword to image_display::display_image
;                        and "leave_mouse" keyword to other routines.  These
;                        changes allow tomo_display:: to scroll though 2-D
;                        slices in a 3-D volume quickly and easily.
;
;       27-APR-2006  MLR Added "tiff" output option
;
;       21-NOV-2007  MLR Added order keyword and order widget, no longer use !order except at startup
;                        Added RETAIN keyword to SCALE_IMAGE, to preserve zoom, center and intensity
;                        scaling.
;                        Added autoscale widget for automatically scaling display range.
;-

    data = reform(data)
    ndims = size(data, /n_dimensions)
    if (ndims ne 2) then t = dialog_message('Data must be 2-D')
    dims = size(data, /dimensions)
    max_size = max(dims)
    if (n_elements(xsize) eq 0) then xsize = max_size > 400 < 1024
    if (n_elements(ysize) eq 0) then ysize = max_size > 400 < 1024
    self.image_window.x_size = xsize
    self.image_window.y_size = ysize
    if (n_elements(order) eq 0) then order=!order
    self.image_window.order = order

    self.row_plot_window.y_size=200
    self.col_plot_window.x_size=200

    top = !d.table_size-1
    ; Don't use decomposed color on 24 bit display, since we want to use
    ; lookup tables.
    if (!d.n_colors gt 256) then device, decomposed=0

    self.widgets.base             = widget_base(column=1, /tlb_kill_request_events)
    row                           = widget_base(self.widgets.base, row=1, /align_center)
    ys = 20
    xs = 10
    col                           = widget_base(row, column=1, /align_top, /frame)
    row1                          = widget_base(col, row=1, /align_top)
    col1                          = widget_base(row1, /column)
    t                             = widget_label(col1, value=' ', ysize=ys)
    t                             = widget_label(col1, value = 'Pixel', ysize=ys)
    t                             = widget_label(col1, value = 'User', ysize=ys)
    col1                          = widget_base(row1, /column)
    t                             = widget_label(col1, value='X')
    self.widgets.x_pixel          = widget_text(col1, xsize=xs)
    self.widgets.x_user           = widget_text(col1, xsize=xs)
    col1                          = widget_base(row1, /column)
    t                             = widget_label(col1, value='Y')
    self.widgets.y_pixel          = widget_text(col1, xsize=xs)
    self.widgets.y_user           = widget_text(col1, xsize=xs)
    col1                          = widget_base(row1, /column)
    t                             = widget_label(col1, value = 'Pixel value')
    self.widgets.pixel_value      = widget_text(col1, xsize=xs)

    col                           = widget_base(row, column=1, /align_top, /frame)
    row1                          = widget_base(col, row=1, /align_top)
    col1                          = widget_base(row1, /column)
    t                             = widget_label(col1, value=' ', ysize=ys)
    t                             = widget_label(col1, value = 'Actual', ysize=ys)
    t                             = widget_label(col1, value = 'Set', ysize=ys)
    col1                          = widget_base(row1, /column)
    t                             = widget_label(col1, value='Min')
    self.widgets.min_actual       = widget_text(col1, xsize=xs)
    self.widgets.min_set          = widget_text(col1, xsize=xs, /edit)
    col1                          = widget_base(row1, /column)
    t                             = widget_label(col1, value='Max')
    self.widgets.max_actual       = widget_text(col1, xsize=xs)
    self.widgets.max_set          = widget_text(col1, xsize=xs, /edit)

    col                           = widget_base(row, column=1, /align_center)
    self.widgets.autoscale        = widget_button(col, value='Autoscale')
    row                           = widget_base(self.widgets.base, row=1)
    base                          = widget_base(row, xsize=self.col_plot_window.x_size, ysize=self.row_plot_window.y_size, column=1)
    col                           = widget_base(base, column=1, /align_center)
    row1                          = widget_base(col, row=1, /frame, /align_center)
    col1                          = widget_base(row1, column=1, /align_center)
    t                             = widget_label(col1, value='Display order')
    self.widgets.order            = widget_droplist(col1, value=['Bottom to top', 'Top to bottom'])
    col1                          = widget_base(row1, col=1, /align_center)
    t                             = widget_label(col1, value='Zoom Mode')
    self.widgets.zoom_mode        = widget_droplist(col1, value=['Replicate', 'Interpolate'])
    row1                          = widget_base(col, row=1, /frame, /align_center)
    self.widgets.new_color_table  = widget_button(row1, value='New color table')
    self.widgets.reset_zoom       = widget_button(row1, value='Reset zoom')
    col1                          = widget_base(base, column=1, /align_center,/frame)
    row1                          = widget_base(col1, row=1)
    t                             = widget_label(col1, value='Output image file')
    row1                          = widget_base(col1, row=1)
    self.widgets.output_width     = cw_field(row1, /row, /integer, xsize=10, title='Pixels (horizontal)', value=2048)
    self.widgets.write_output_file = widget_button(col1, value='Save File')
    self.widgets.row_plot         = widget_window(row, xsize=xsize, ysize=self.row_plot_window.y_size)
    row                           = widget_base(self.widgets.base, row=1)
    self.widgets.col_plot         = widget_window(row, ysize=ysize, xsize=self.col_plot_window.x_size)
    self.widgets.image            = widget_window(row, xsize=xsize, ysize=ysize, /motion_events, /button_events)

    widget_control, self.widgets.base, /realize
    widget_control, self.widgets.image, get_value=window
    self.image_window.window_obj = window
    widget_control, self.widgets.row_plot, get_value=window
    self.row_plot_window.window_obj = window
    widget_control, self.widgets.col_plot, get_value=window
    self.col_plot_window.window_obj = window

    self->new_image, data, xdist=xdist, ydist=ydist, min=min, max=max, $
                     title=title, order=order
    widget_control, self.widgets.base, set_uvalue=self

    xmanager, 'image_display', self.widgets.base, /no_block
    return, 1
end

pro image_display::cleanup
    ptr_free, self.image_data.raw_data
    ptr_free, self.image_data.x_dist
    ptr_free, self.image_data.y_dist
end

pro image_display__define, data, xsize=xsize, ysize=ysize

    widgets={ image_display_widgets, $
        base: 0L, $
        image: 0L, $
        row_plot: 0L, $
        col_plot: 0L, $
        x_pixel: 0L, $
        y_pixel: 0L, $
        x_user: 0L, $
        y_user: 0L, $
        pixel_value: 0L, $
        min_actual: 0L, $
        max_actual: 0L, $
        min_set: 0L, $
        max_set: 0L, $
        order:     0L, $
        autoscale: 0L, $
        zoom_mode: 0L, $
        new_color_table: 0L, $
        reset_zoom: 0L, $
        output_width: 0L, $
        write_output_file: 0L, $
        exit: 0L $
    }

    image_window = {image_display_image_window, $
        window_obj: obj_new(), $
        image_obj: obj_new(), $
        x_size: 0L, $
        y_size: 0L, $
        black_level: 0.0, $
        white_level: 0.0, $
        interpolate: 0L, $
        order:    0L $
    }

    col_plot_window = {image_display_plot, $
        window_obj: obj_new(), $
        plot_obj: obj_new(), $
        arrow_obj: obj_new(), $
        x_size: 0L, $
        y_size: 0L $
    }

    row_plot_window = {image_display_plot}

    image_data = {image_display_image_data, $
        raw_data: ptr_new(), $
        oformats: strarr(4), $
        title:    '', $
        x_size: 0L, $
        y_size: 0L, $
        x_dist: ptr_new(), $
        y_dist: ptr_new() $
    }

    image_display = {image_display, $
        widgets: widgets, $
        image_window: image_window, $
        row_plot_window: row_plot_window, $
        col_plot_window: col_plot_window, $
        image_data: image_data $
    }
end


