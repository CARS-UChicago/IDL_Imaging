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

;------------------
pro image_display::plot_profiles, x_mouse, y_mouse

  if (n_elements(x_mouse) eq 0) then x_mouse = self.image_window.x_size/2
  if (n_elements(y_mouse) eq 0) then y_mouse = self.image_window.y_size/2
  coords = self.image_window.image_obj->convertcoord(x_mouse, y_mouse, /device, /to_data)
  xc = coords[0]
  yc = coords[1]
  if (self.image_window.order eq 1) then yc = self.image_data.y_size - yc - 1
  xc = xc > 0 < (self.image_data.x_size-1)
  yc = yc > 0 < (self.image_data.y_size-1)
  x_user = (*self.image_data.x_dist)[xc]
  y_user = (*self.image_data.y_dist)[yc]
  
  widget_control, self.widgets.x_pixel, set_value = string(xc, format='(i)')
  widget_control, self.widgets.y_pixel, set_value = string(yc, format='(i)')
  widget_control, self.widgets.x_user, set_value = string(x_user)
  widget_control, self.widgets.y_user, set_value = string(y_user)
  widget_control, self.widgets.pixel_value, set_value = string((*self.image_data.raw_data)[xc, yc])

  x_axis = *self.image_data.x_dist
  y_axis = *self.image_data.y_dist
  x_min = (self.image_window.image_obj).xrange[0] > 0 < (self.image_data.x_size-1)
  x_max = (self.image_window.image_obj).xrange[1] > 0 < (self.image_data.x_size-1)
  y_min = (self.image_window.image_obj).yrange[0] > 0 < (self.image_data.y_size-1)
  y_max = (self.image_window.image_obj).yrange[1] > 0 < (self.image_data.y_size-1)
  if (self.image_window.order eq 1) then begin
    y_max = self.image_data.y_size - y_max - 1
    y_min = self.image_data.y_size - y_min - 1
  endif
  black_level = (self.image_window.image_obj).min_value
  white_level = (self.image_window.image_obj).max_value

  xnorm = (xc - x_min) / (x_max - x_min)
  ynorm = (yc - y_min) / (y_max - y_min)
  plot = self.row_plot_window.plot_obj
  y = reform((*self.image_data.raw_data)[*, yc])
  plot.setdata, x_axis, y
  plot.xrange = [x_axis[x_min], x_axis[x_max]]
  plot.yrange = [black_level, white_level]
  arrow = self.row_plot_window.arrow_obj
  arrow.position = ([xnorm, 0, xnorm, 1])
  
  plot = self.col_plot_window.plot_obj
  x = reform((*self.image_data.raw_data)[xc, *])
  plot.setdata, x, y_axis
  plot.xrange = [black_level, white_level]
  plot.yrange = [y_axis[y_min], y_axis[y_max]]
  arrow = self.col_plot_window.arrow_obj
  arrow.position = ([0, ynorm, 1, ynorm])

end

pro image_display::select_direction, direction, data, xdist, ydist, title, slice=slice
  widget_control, self.widgets.slice_direction, set_droplist_select=direction
  case direction of
    0: begin
      if (n_elements(slice) eq 0) then slice= self.volume_data.x_size/2
      xdist = *self.volume_data.y_dist
      ydist = *self.volume_data.z_dist
      widget_control, self.widgets.slice_slider, set_slider_max = self.volume_data.x_size-1
    end
    1: begin
      if (n_elements(slice) eq 0) then slice= self.volume_data.y_size/2
      xdist = *self.volume_data.x_dist
      ydist = *self.volume_data.z_dist
      widget_control, self.widgets.slice_slider, set_slider_max = self.volume_data.y_size-1
    end
    2: begin
      if (n_elements(slice) eq 0) then slice= self.volume_data.z_size/2
      xdist = *self.volume_data.x_dist
      ydist = *self.volume_data.y_dist
      widget_control, self.widgets.slice_slider, set_slider_max = self.volume_data.z_size-1
    end
  endcase
  self->select_slice, slice, data, title
  widget_control, self.widgets.slice_slider, set_value=slice
end

pro image_display::select_slice, slice, data, title
  direction = widget_info(self.widgets.slice_direction, /droplist_select)
  case direction of
    0: begin
      data = reform((*self.volume_data.pdata)[slice, *, *])
    end
    1: begin
      data = reform((*self.volume_data.pdata)[*, slice, *])
    end
    2: begin
      data = reform((*self.volume_data.pdata)[*, *, slice])
    end
  endcase
  widget_control, self.widgets.slice_number, set_value=strtrim(slice,2)
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

;+
; NAME:
;  IMAGE_DISPLAY::SET_IMAGE_DATA
;
; PURPOSE:
;   This procedure is used to display a new 2-D slice.
;   It should only be called when the data is the same dimensions as the data
;   passed when the IMAGE_DISPLAY object was created, or when IMAGE_DISPLAY::NEW_IMAGE was called.
;
; CALLING SEQUENCE:
;   IMAGE_DISPLAY::SET_IMAGE_DATA, DATA, TITLE=TITLE, MIN=MIN, MAX=MAX
;
; INPUTS:
;   DATA: The 2-D image to display. It must have the same dimensions as the currently displayed image.
;  
; KEYWORD PARAMETERS
;   TITLE:
;     The title to be displayed on the top of the main IMAGE_DISPLAY window.
;     Default: 'IDL Image Display'
;
;   MIN:
;     The minimum intensity to be displayed.  Values less than this are displayed as the first color table entry.
;     Default: current minimum display intensity is not changed.
;
;   MAX:
;     The maximum intensity to be displayed.  Values greater than this are displayed as the last color table entry.
;     Default: current maximum display intensity is not changed.
;
; PROCEDURE:
;   - Updates the actual min and max intensity widgets.
;   - Sets the displayed min and max values if specified
;   - Updates the displayed image
;   - Calls IMAGE_DISPLAY::PLOT_PROFILES to update the row and column profile plots
;-

pro image_display::set_image_data, data, title=title, min=min, max=max

  if (n_elements(title) eq 0) then title = 'IDL Image Display'
  self.image_data.title = title
  widget_control, self.widgets.base, tlb_set_title=title

  ndims = size(data, /n_dimensions)
  if (ndims ne 2) then data = reform(data)
  min_data = min(data, max=max_data)
  widget_control, self.widgets.min_actual, set_value = string(min_data)
  widget_control, self.widgets.max_actual, set_value = string(max_data)

  if (n_elements(min) ne 0) then  begin
    self.image_window.black_level = min
    widget_control, self.widgets.min_set, set_value = string(min)
  endif

  if (n_elements(max) ne 0) then begin
    self.image_window.white_level = max
    widget_control, self.widgets.max_set, set_value = string(max)
  endif
 
  ptr_free, self.image_data.raw_data
  self.image_data.raw_data = ptr_new(data)
  self.image_window.image_obj->setdata, data, order=self.image_window.order
  self->update_image
  self->plot_profiles
end

;+
; NAME:
;   IMAGE_DISPLAY::NEW_IMAGE
;
; PURPOSE:
;   This routine displays a new image in an existing image_display object.
;
; CALLING SEQUENCE:
;   IMAGE_DISPLAY::NEW_IMAGE, DATA, XDIST=XDIST, YDIST=YDIST,  MIN=MIN, MAX=MAX, TITLE=TITLE, ORDER=ORDER
;
; INPUTS:
;   DATA: The 2-D image to display. It must have the same dimensions as the currently displayed image.
;
; KEYWORD PARAMETERS:
;   XDIST:
;     An array containing the user units ("distance") of the X axis pixels.  Dimensions must be same as xsize of DATA.
;     Default: Pixel number in X direction.
;
;   YDIST:
;     An array containing the user units ("distance") of the Y axis pixels.  Dimensions must be same as ysize of DATA.
;     Default: Pixel number in Y direction.
;
;   MIN:
;     The minimum display intensity.  Default=min(Data).
;
;   MAX:
;     The maximum display intensity.  Default=max(Data).
;
;   TITLE:
;     The title to be displayed on the top of the main IMAGE_DISPLAY window.
;   
;   ORDER:  
;     The order in which to display the image.
;       0=bottom to top
;       1=top to bottom
;       Default = Existing order
;       
; PROCEDURE:
;   - Sets the image ORDER if specified.
;   - Sets XDIST and YDIST to the default or specified values.
;   - Deletes the existing IMAGE object and creates a new one.
;   - Deletes the existing row and column PLOT objects and creates new ones.
;   - Calls IMAGE_DISPLAY::SET_IMAGE_DATA to display the image and update the row and column profile plots
;-
pro image_display::new_image, data, $
                              xdist=xdist, ydist=ydist, $
                              min=min, max=max, $
                              title=title, order=order

  data = reform(data)
  ndims = size(data, /n_dimensions)
  if (ndims ne 2) then t = dialog_message('Data must be 2-D')
  dims = size(data, /dimensions)
  self.image_data.x_size = dims[0]
  self.image_data.y_size = dims[1]
  scale_factor = float(max(dims))/min(dims)

  if (n_elements(order) ne 0) then begin
    self.image_window.order = order
    widget_control, self.widgets.order, set_droplist_select=self.image_window.order
  endif

  if (n_elements(xdist) eq 0) then xdist = findgen(self.image_data.x_size)
  if (n_elements(ydist) eq 0) then ydist = findgen(self.image_data.y_size)

  if (n_elements(xdist) ne self.image_data.x_size) or $
     (n_elements(ydist) ne self.image_data.y_size) then $
    t = dialog_message('Size of xdist, ydist, and zdist must match size of image')
  ptr_free, self.image_data.x_dist
  ptr_free, self.image_data.y_dist
  self.image_data.x_dist = ptr_new(xdist)
  self.image_data.y_dist = ptr_new(ydist)

  if (obj_valid(self.image_window.image_obj)) then (self.image_window.image_obj).erase
  (self.image_window.window_obj).select
  self.image_window.image_obj = image(data, /current, order=self.image_window.order, margin=0)

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

  min_data = min(data, max=max_data)
  if (n_elements(min) eq 0) then min = min_data
  if (n_elements(max) eq 0) then max = max_data

  self->set_image_data, data, title=title, min=min, max=max
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

    self.widgets.slice_direction: begin
      self->select_direction, event.index, data, xdist, ydist, title
      widget_control, self.widgets.min_set, get_value=min
      widget_control, self.widgets.max_set, get_value=max
      self->new_image, data, xdist=xdist, ydist=ydist, min=min, max=max, title=title
    end

    self.widgets.slice_slider: begin
      self->select_slice, event.value, data, title
      self->set_image_data, data, title=title
    end

    self.widgets.slice_number: begin
      widget_control, self.widgets.slice_number, get_value=value
      self->select_slice, value, data, title
      widget_control, self.widgets.slice_slider, set_value=value
      self->set_image_data, data, title=title
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
                                    xdist=xdist, ydist=ydist, zdist=zdist, $
                                    min=min, max=max, $
                                    title=title, order=order
;+
; NAME:
;   IMAGE_DISPLAY::INIT
;
; PURPOSE:
;   This function initializes an object of class IMAGE_DISPLAY.  
;   It is not called directly, but is called indirectly when a new object of
;   class IMAGE_DISPLAY is created via OBJ_NEW('IMAGE_DISPLAY')
;
;   The IMAGE_DISPLAY object is a GUI display which provides interactive
;   pan, zoom and scroll, live update of row and column profiles, etc.
;
; CALLING SEQUENCE:
;    OBJ = OBJ_NEW('IMAGE_DISPLAY', DATA, KEYWORD=VALUE, ...)
;
; INPUTS:
;   DATA:   
;     A 2-D or 3-D array to be displayed.  
;     If DATA is 3-D then widgets at the bottom of the GUI are used to select the direction (to be displayed X, Y, Z),
;     and the slice in that direction to be displayed.
;
; KEYWORD PARAMETERS:
;   XSIZE:  
;     The number of pixels horizontally in the image window.
;     Default is the greater of 400 pixels or the xsize of DATA.
;
;   YSIZE:  
;     The number of pixels vertically in the image window.
;     Default is the greater of 400 pixels or the ysize of Data.
;
;   XDIST:
;     An array containing the user units ("distance") of the X axis pixels.  Dimensions must be same as X size of DATA.
;     Default: Pixel number in X direction.
;
;   YDIST:
;     An array containing the user units ("distance") of the Y axis pixels.  Dimensions must be same as Y size of DATA.
;     Default: Pixel number in Y direction.
;
;   ZDIST:
;     An array containing the user units ("distance") of the Z axis pixels.  Dimensions must be same as Z size of DATA.
;     Default: Pixel number in Z direction.
;     Only used if data is 3-D.
;
;   MIN:
;     The minimum display intensity.  Default=min(DATA).
;
;   MAX:
;     The maximum display intensity.  Default=max(DATA).
;
;   TITLE:
;     The title to be displayed on the top of the main IMAGE_DISPLAY window.
;
;   ORDER:
;     The order in which to display the image.
;       0=bottom to top
;       1=top to bottom
;       Default = Existing order
;
; OUTPUTS:
;   This function returns 1 to indicate that the object was successfully created.
;-

  ndims = size(data, /n_dimensions)
  if ((ndims lt 2) or (ndims gt 3)) then t = dialog_message('Data must be 2-D or 3-D')
  dims = size(data, /dimensions)
  if (min(dims) eq 1) then begin
    data = reform(data)
    ndims = size(data, /n_dimensions)
    dims = size(data, /dimensions)
  endif
  self.volume_data.x_size = dims[0]
  self.volume_data.y_size = dims[1]
  if (ndims eq 3) then self.volume_data.z_size = dims[2] else self.volume_data.z_size = 1
  self.volume_data.pdata = ptr_new(data)

  if (n_elements(xdist) eq 0) then xdist = findgen(self.volume_data.x_size)
  if (n_elements(ydist) eq 0) then ydist = findgen(self.volume_data.y_size)
  if (n_elements(zdist) eq 0) then zdist = findgen(self.volume_data.z_size)

  if (n_elements(xdist) ne self.volume_data.x_size) or $
     (n_elements(ydist) ne self.volume_data.y_size) or $
     (n_elements(zdist) ne self.volume_data.z_size) then $
    t = dialog_message('Size of xdist, ydist, and zdist must match size of image')
  self.volume_data.x_dist = ptr_new(xdist)
  self.volume_data.y_dist = ptr_new(ydist)
  self.volume_data.z_dist = ptr_new(zdist)

  max_size = max(dims)
  device, get_screen_size=scr_size
  scr_size = scr_size - 450
  r = float(dims)/scr_size
  if (r[1] gt r[0]) then begin
    ys = scr_size[1]
    xs = float(scr_size[1]) * dims[0] / dims[1]
  endif else begin
    xs = scr_size[0]
    ys = float(scr_size[0]) * dims[1] / dims[0]
  endelse
  d = dims * min(r)
  if (n_elements(xsize) eq 0) then xsize = max_size > 400 < xs
  if (n_elements(ysize) eq 0) then ysize = max_size > 400 < ys
  self.image_window.x_size = xsize
  self.image_window.y_size = ysize
  if (n_elements(order) eq 0) then order=!order
  self.image_window.order = order

  plot_height=190

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
  base                          = widget_base(row, xsize=plot_height, ysize=plot_height, column=1)
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
  self.widgets.row_plot         = widget_window(row, xsize=xsize, ysize=plot_height)
  row                           = widget_base(self.widgets.base, row=1)
  self.widgets.col_plot         = widget_window(row, ysize=ysize, xsize=plot_height)
  self.widgets.image            = widget_window(row, xsize=xsize, ysize=ysize, /motion_events, /button_events)

  self.widgets.xyz_base         = widget_base(self.widgets.base, row=1, /align_left)
  col                           = widget_base(self.widgets.xyz_base, column=1, /align_top, xsize=plot_height)
  row1                          = widget_base(col, row=1, /align_left)
  choices = ['X', 'Y', 'Z']
  t                             = widget_label(row1, value='Direction:')
  self.widgets.slice_direction  = widget_droplist(row1, value=choices, uvalue=choices, /align_center)
  t                             = widget_label(row1, value='  Slice:')
  self.widgets.slice_number     = widget_text(row1, xsize=6, /edit)
  col                           = widget_base(self.widgets.xyz_base, column=1, /align_top)
  self.widgets.slice_slider     = widget_slider(col, value=100, min=0, max=100, xsize=xsize, /suppress_value)

  if (self.volume_data.z_size eq 1) then widget_control, self.widgets.xyz_base, sensitive=0
  widget_control, self.widgets.base, /realize
  widget_control, self.widgets.image, get_value=window
  self.image_window.window_obj = window
  widget_control, self.widgets.row_plot, get_value=window
  self.row_plot_window.window_obj = window
  widget_control, self.widgets.col_plot, get_value=window
  self.col_plot_window.window_obj = window

  self->select_direction, 2, image_data, xdist, ydist
  self->new_image, image_data, xdist=xdist, ydist=ydist, min=min, max=max, title=title, order=order
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
    base:              0L, $
    image:             0L, $
    row_plot:          0L, $
    col_plot:          0L, $
    x_pixel:           0L, $
    y_pixel:           0L, $
    x_user:            0L, $
    y_user:            0L, $
    pixel_value:       0L, $
    min_actual:        0L, $
    max_actual:        0L, $
    min_set:           0L, $
    max_set:           0L, $
    order:             0L, $
    autoscale:         0L, $
    zoom_mode:         0L, $
    new_color_table:   0L, $
    reset_zoom:        0L, $
    output_width:      0L, $
    write_output_file: 0L, $
    xyz_base:          0L, $
    slice_direction:   0L, $
    slice_number:      0L, $
    slice_slider:      0L  $
  }

  image_window = {image_display_image_window, $
    window_obj:  obj_new(), $
    image_obj:   obj_new(), $
    x_size:      0L,        $
    y_size:      0L,        $
    black_level: 0.0,       $
    white_level: 0.0,       $
    interpolate: 0L,        $
    order:       0L         $
  }

  col_plot_window = {image_display_plot, $
    window_obj: obj_new(), $
    plot_obj:   obj_new(), $
    arrow_obj:  obj_new()  $
  }

  row_plot_window = {image_display_plot}

  image_data = {image_display_image_data, $
    raw_data: ptr_new(), $
    title:    '',        $
    x_size:   0L,        $
    y_size:   0L,        $
    x_dist:   ptr_new(), $
    y_dist:   ptr_new()  $
  }

  volume_data = {image_display_volume_data, $
    pdata:    ptr_new(), $
    title:    '',        $
    x_size:   0L,        $
    y_size:   0L,        $
    z_size:   0L,        $
    x_dist:   ptr_new(), $
    y_dist:   ptr_new(), $
    z_dist:   ptr_new()  $
 }

 image_display = {image_display,       $ 
    widgets:          widgets,         $
    image_window:     image_window,    $
    row_plot_window:  row_plot_window, $
    col_plot_window:  col_plot_window, $
    image_data:       image_data,      $
    volume_data:      volume_data      $
  }
end
