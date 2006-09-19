;+
; NAME:
;   VOLUME_RENDER
;
; PURPOSE:
;   This procedure is a general purpose GUI-based volume rendering routine.
;   It allows setting of cutting planes, color and opacity tables, etc.
;
; CATEGORY:
;   Imaging
;
; CALLING SEQUENCE:
;   VOLUME_RENDER, Data
;
; INPUTS:
;   Data:   A 3-D BYTE array to be displayed
;
; PROCEDURE:
;   This procedure is a GUI front-end for the IDLgrVolume object.
;   See the documentation on IDLgrVOLUME for more information.
;
; RESTRICTIONS:
;   This procedure is a work in progress, and additional features will be added
;   as time permits.  In particular a better opacity table editor is needed.
;
; EXAMPLE:
;       IDL> a = bingen(200, 200, 200)
;       IDL> volume_render, a
;
; MODIFICATION HISTORY:
;   Written by:     Mark Rivers (12-MAR-1999)
;                   This procedure was based on the d_volrendr.pro from the
;                   IDL demos, generalized and with additional features
;
;-

;----------------------------------------------------------------------------
;
; PURPOSE  Callback routine from XLOADCT when RGB color tables are changed
;
pro d_volrendrRGBCallback, data=pstate
    tvlct, r, g, b, /get
    (*pState).rvolume->SetProperty, RGB_TABLE0=[[r], [g], [b]]
    d_volrendrDraw, *pState, $
        QUALITY=([0,(*pState).render_quality])((*pState).auto_render)
end

;----------------------------------------------------------------------------
;
; PURPOSE  Callback routine from XLOADCT when opacity color table is changed
;
pro d_volrendrOpacityCallback, data=pstate
    tvlct, r, g, b, /get
    (*pState).rvolume->SetProperty, OPACITY_TABLE0=r
    d_volrendrDraw, *pState, $
        QUALITY=([0,(*pState).render_quality])((*pState).auto_render)
end

;----------------------------------------------------------------------------
;
; PURPOSE  Invoke draw method for our application's IDLgrWindow.
;
pro d_volrendrDraw, state, QUALITY=quality

if state.suppress_draws then $
    RETURN

if N_ELEMENTS(quality) eq 0 then begin
    state.rWindow->GetProperty, QUALITY=quality
    end $
else begin
    state.rWindow->SetProperty, QUALITY=quality
    end
;
;Silently flush any accumulated math error.
;
void = check_math()
;
;Silently accumulate any subsequent math errors.
;
orig_except = !except
!except = 0
;
;Draw.
;
case quality of
    0: begin
        state.rView->Add, state.rToysModel
        state.rWindow->Draw, state.rView
        state.rView->Remove, state.rToysModel
        end
    1: begin
        tic = systime(1)
        WIDGET_CONTROL, state.wBase, SENSITIVE=0
        WIDGET_CONTROL, /HOURGLASS

        state.rView->Add, state.rToysModel, POSITION=0
        state.rWindow->Draw, state.rView, /CREATE_INSTANCE
        state.rView->Remove, state.rToysModel

        state.an_instance_exists = 1b ; true from now on.

        state.rTransparentView->Add, state.rToysModel
        state.rWindow->Draw, state.rTransparentView, /DRAW_INSTANCE
        state.rTransparentView->Remove, state.rToysModel
;       print, systime(1) - tic, ' Seconds'
        end
    2: begin
        tic = systime(1)
        WIDGET_CONTROL, state.wBase, SENSITIVE=0
        WIDGET_CONTROL, /HOURGLASS

        state.rView->Add, state.rToysModel, POSITION=0
        state.rWindow->Draw, state.rView, /CREATE_INSTANCE
        state.rView->Remove, state.rToysModel

        state.an_instance_exists = 1b ; true from now on.

        state.rWindow->Draw, state.rTransparentView, /DRAW_INSTANCE
;       print, systime(1) - tic, ' Seconds'
        end
    endcase

;
;Ignore any accumulated floating underflow errors.  (They are benign).
;Display any other accumulated math errors.
;
floating_point_underflow = 32 ; Per IDL Reference Guide: see Check_Math().
status = check_math()         ; Get status and reset.
if (status and not floating_point_underflow) ne 0 then $
    print, 'IDL Check_Math() error: ' + strtrim(status, 2)
;
;Restore original math error behavior.
;
!except = orig_except
;
state.cursor_stale = 0b ; 3D grapics cursor appearance is up to date.
WIDGET_CONTROL, state.wBase, SENSITIVE=1

end
;----------------------------------------------------------------------------
;
; PURPOSE  Refresh our applications's IDLgrWindow
;
pro d_volrendrRefresh, state

state.rWindow->GetProperty, QUALITY=quality
if state.an_instance_exists then $
    case quality of
        0: d_volrendrDraw, state, QUALITY=quality
        1: begin
            state.rTransparentView->Add, state.rToysModel, POSITION=0
            state.rWindow->Draw, state.rTransparentView, /DRAW_INSTANCE
            state.rTransparentView->Remove, state.rToysModel
            end
        2: state.rWindow->Draw, state.rTransparentView, /DRAW_INSTANCE
        endcase $
else $
    d_volrendrDraw, state, QUALITY=quality

end
;----------------------------------------------------------------------------
;
; PURPOSE  Toggle the buton state between off and on.
;
function d_volrendrToggleState, $
    widgetID         ; IN: button widget identifer.

WIDGET_CONTROL, widgetID, GET_VALUE=name

s = STRPOS(name,'(off)')
if (s NE -1) then begin
    STRPUT, name, '(on )', s
    ret = 1
    end $
else begin
    s = STRPOS(name,'(on )')
    STRPUT, name, '(off)', s
    ret = 0
    end

WIDGET_CONTROL, widgetID, SET_VALUE=name
RETURN, ret
end

;----------------------------------------------------------------------------
;
; PURPOSE  Handle GUI events.
;
pro d_volrendrEvent, $
    event              ; IN: event structure.

WIDGET_CONTROL, event.top, GET_UVALUE=pState

if (TAG_NAMES(event, /STRUCTURE_NAME) eq  $
    'WIDGET_KILL_REQUEST') then begin
    WIDGET_CONTROL, event.top, /DESTROY
    RETURN
    endif
;
;Get the event user value.
;
WIDGET_CONTROL, event.id, GET_UVALUE=uval
;
;Branch to the correct event user value.
;
case uval of
    'HOTKEY' : begin ; Left Mouse-Button Mode.
        case STRUPCASE(event.ch) of
            'U': begin ; unconstrained rotation
                (*pState).lmb_scale = 0
                (*pState).rModelArray[0]->SetProperty, CONSTRAIN=0
                (*pState).rModelArray[1]->SetProperty, CONSTRAIN=0
                WIDGET_CONTROL, (*pState).wLMBMode, SET_DROPLIST_SELECT=0
                end
            'X': begin
                (*pState).lmb_scale = 0
                (*pState).axis = 0
                (*pState).rModelArray[0]->SetProperty, AXIS=0, /CONSTRAIN
                (*pState).rModelArray[1]->SetProperty, AXIS=0, /CONSTRAIN
                WIDGET_CONTROL, (*pState).wLMBMode, SET_DROPLIST_SELECT=1
                end
            'Y': begin
                (*pState).lmb_scale = 0
                (*pState).axis = 1
                (*pState).rModelArray[0]->SetProperty, AXIS=1, /CONSTRAIN
                (*pState).rModelArray[1]->SetProperty, AXIS=1, /CONSTRAIN
                WIDGET_CONTROL, (*pState).wLMBMode, SET_DROPLIST_SELECT=2
                end
            'Z': begin
                (*pState).lmb_scale = 0
                (*pState).axis = 2
                (*pState).rModelArray[0]->SetProperty, AXIS=2, /CONSTRAIN
                (*pState).rModelArray[1]->SetProperty, AXIS=2, /CONSTRAIN
                WIDGET_CONTROL, (*pState).wLMBMode, SET_DROPLIST_SELECT=3
                end
            'R': begin
                (*pState).lmb_scale = 0
                (*pState).axis = 0
                (*pState).rModelArray[0]->SetProperty, AXIS=0, CONSTRAIN=2
                (*pState).rModelArray[1]->SetProperty, AXIS=0, CONSTRAIN=2
                WIDGET_CONTROL, (*pState).wLMBMode, SET_DROPLIST_SELECT=4
                end
            'G': begin
                (*pState).lmb_scale = 0
                (*pState).axis = 1
                (*pState).rModelArray[0]->SetProperty, AXIS=1, CONSTRAIN=2
                (*pState).rModelArray[1]->SetProperty, AXIS=1, CONSTRAIN=2
                WIDGET_CONTROL, (*pState).wLMBMode, SET_DROPLIST_SELECT=5
                end
            'B': begin
                (*pState).lmb_scale = 0
                (*pState).axis = 2
                (*pState).rModelArray[0]->SetProperty, AXIS=2, CONSTRAIN=2
                (*pState).rModelArray[1]->SetProperty, AXIS=2, CONSTRAIN=2
                WIDGET_CONTROL, (*pState).wLMBMode, SET_DROPLIST_SELECT=6
                end
            'S': begin
                (*pState).lmb_scale = 1
                WIDGET_CONTROL, (*pState).wLMBMode, SET_DROPLIST_SELECT=7
                end
            else:
            endcase
        (*pState).rModelArray[0]->GetProperty, CONSTRAIN=constrain
        if constrain eq 2 then begin
            if d_volrendrToggleState((*pState).wAxesButton) eq 0 then $
                void = d_volrendrToggleState((*pState).wAxesButton) $
            else begin
                (*pState).rOtherObjectArray[7]->SetProperty, HIDE=0
                d_volrendrDraw, *pState, $
                    QUALITY= $
                        ([0,(*pState).render_quality])((*pState).auto_render)
                end
            end
        end
    'LMBMODE' : begin ; Left Mouse-Button Mode.
        case event.index of
            0: begin ; unconstrained rotation
                (*pState).lmb_scale = 0
                (*pState).axis = 3
                (*pState).rModelArray[0]->SetProperty, CONSTRAIN=0
                (*pState).rModelArray[1]->SetProperty, CONSTRAIN=0
                end
            1: begin
                (*pState).lmb_scale = 0
                (*pState).axis = 0
                (*pState).rModelArray[0]->SetProperty, AXIS=0, /CONSTRAIN
                (*pState).rModelArray[1]->SetProperty, AXIS=0, /CONSTRAIN
                (*pState).screen_rotate = 1
                end
            2: begin
                (*pState).lmb_scale = 0
                (*pState).axis = 1
                (*pState).rModelArray[0]->SetProperty, AXIS=1, /CONSTRAIN
                (*pState).rModelArray[1]->SetProperty, AXIS=1, /CONSTRAIN
                end
            3: begin
                (*pState).lmb_scale = 0
                (*pState).axis = 2
                (*pState).rModelArray[0]->SetProperty, AXIS=2, /CONSTRAIN
                (*pState).rModelArray[1]->SetProperty, AXIS=2, /CONSTRAIN
                end
            4: begin
                (*pState).lmb_scale = 0
                (*pState).axis = 0
                (*pState).rModelArray[0]->SetProperty, AXIS=0, CONSTRAIN=2
                (*pState).rModelArray[1]->SetProperty, AXIS=0, CONSTRAIN=2
                end
            5: begin
                (*pState).lmb_scale = 0
                (*pState).axis = 1
                (*pState).rModelArray[0]->SetProperty, AXIS=1, CONSTRAIN=2
                (*pState).rModelArray[1]->SetProperty, AXIS=1, CONSTRAIN=2
                end
            6: begin
                (*pState).lmb_scale = 0
                (*pState).axis = 2
                (*pState).rModelArray[0]->SetProperty, AXIS=2, CONSTRAIN=2
                (*pState).rModelArray[1]->SetProperty, AXIS=2, CONSTRAIN=2
                end
            7: begin
                (*pState).lmb_scale = 1
                end
            else:
            endcase
        (*pState).rModelArray[0]->GetProperty, CONSTRAIN=constrain
        if constrain eq 2 then begin
            if d_volrendrToggleState((*pState).wAxesButton) eq 0 then $
                void = d_volrendrToggleState((*pState).wAxesButton) $
            else begin
                (*pState).rOtherObjectArray[7]->SetProperty, HIDE=0
                d_volrendrDraw, *pState, $
                    QUALITY= $
                        ([0,(*pState).render_quality])((*pState).auto_render)
                end
            end
        end

    'RENDER_MODE' : begin ; Render mode
        (*pState).rVolume->SetProperty, COMPOSITE_FUNCTION=event.index
        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)
     end

    'XMIN' : begin
        (*pState).rVolume->GetProperty, XRANGE=xrange
        (*pState).rVolume->GetProperty, CUTTING_PLANE=cutting_plane
        cutting_plane[*,0] = [1,0,0,-(event.value / 100.) * xrange[1]]
        (*pState).rVolume->SetProperty, CUTTING_PLANE=cutting_plane
        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)
        end

    'XMAX' : begin
        (*pState).rVolume->GetProperty, XRANGE=xrange
        (*pState).rVolume->GetProperty, CUTTING_PLANE=cutting_plane
        cutting_plane[*,1] = [-1,0,0,(event.value / 100.) * xrange[1]]
        (*pState).rVolume->SetProperty, CUTTING_PLANE=cutting_plane
        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)
        end

    'YMIN' : begin
        (*pState).rVolume->GetProperty, YRANGE=yrange
        (*pState).rVolume->GetProperty, CUTTING_PLANE=cutting_plane
        cutting_plane[*,2] = [0,1,0, -(event.value / 100.) * yrange[1]]
        (*pState).rVolume->SetProperty, CUTTING_PLANE=cutting_plane
        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)
        end

    'YMAX' : begin
        (*pState).rVolume->GetProperty, YRANGE=yrange
        (*pState).rVolume->GetProperty, CUTTING_PLANE=cutting_plane
        cutting_plane[*,3] = [0,-1,0, (event.value / 100.) * yrange[1]]
        (*pState).rVolume->SetProperty, CUTTING_PLANE=cutting_plane
        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)
        end

    'ZMIN' : begin
        (*pState).rVolume->GetProperty, ZRANGE=zrange
        (*pState).rVolume->GetProperty, CUTTING_PLANE=cutting_plane
        cutting_plane[*,4] = [0,0,1, -(event.value / 100.) * zrange[1]]
        (*pState).rVolume->SetProperty, CUTTING_PLANE=cutting_plane
        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)
        end

    'ZMAX' : begin
        (*pState).rVolume->GetProperty, ZRANGE=zrange
        (*pState).rVolume->GetProperty, CUTTING_PLANE=cutting_plane
        cutting_plane[*,5] = [0,0,-1, (event.value / 100.) * zrange[1]]
        (*pState).rVolume->SetProperty, CUTTING_PLANE=cutting_plane
        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)
        end

;
;   Render the image and display it.
;
    'RENDER' : begin
        d_volrendrDraw, *pState, QUALITY=(*pState).render_quality
        end
;
;   Generate a png of the image. AL 2006
;
    'PNG' : begin
        d_volrendrDraw, *pState, QUALITY=(*pState).render_quality
        (*pState).rWindow->GetProperty, IMAGE_DATA=img
        png_file=dialog_pickfile(/write, filter='*.png', title='File to save PNG to...')
        WRITE_PNG, png_file, img
        end


;
;   Set on or off the auto rendering property.
;
    'AUTORENDER' : begin
        (*pState).auto_render = event.select
        WIDGET_CONTROL, (*pState).wRenderButton, $
            SENSITIVE=([1, 0])(event.select)
        if (*pState).auto_render then begin
            (*pState).rWindow->GetProperty, QUALITY=current_quality

            (*pState).rOtherObjectArray[2]->GetProperty, $ ; 3D Cursor.
                HIDE=hide

            if ((hide eq 0) and (*pState).cursor_stale) $
            or (current_quality ne (*pState).render_quality) then $
                d_volrendrDraw, *pState, QUALITY=(*pState).render_quality
            end
        end   ; of AUTORENDER

;
;   Turn on or off the lights.
;
    'LIGHTING': begin
        (*pState).rVolume->SetProperty, LIGHTING_MODEL=event.select

        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)

        end   ; of LIGHTING
;
;   Hide or show the wire box object.
;
    'WIREBOX' : begin
        j = d_volrendrToggleState(event.id)
        (*pState).rOtherObjectArray[0]->SetProperty, HIDE=1-j

        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)

        end

;   Show or hide the axis object.
;
    'AXES' : begin
        j = d_volrendrToggleState(event.id)
        (*pState).rOtherObjectArray[7]->SetProperty,HIDE=1-j
        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)
        end
;
;   Show or hide the 3D cursor object.

    'CURSOR' : begin
        j = d_volrendrToggleState(event.id)
        (*pState).rOtherObjectArray[2]->SetProperty,HIDE=1-j
        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)
        end

;
;   Modify the RGB color table

    'RGB' : begin
        xloadct, updatecallback='d_volrendrRGBCallback', updatecbdata=pState
    end

    'OPACITY' : begin
        xloadct, updatecallback='d_volrendrOpacityCallback', updatecbdata=pState
    end


;   Set the user-indicated render step.
;
    'XSTEP' : begin
        (*pState).rvolume->GetProperty, RENDER_STEP=render_step
        render_step[0] = event.value + 1
        (*pState).rVolume->SetProperty, RENDER_STEP=render_step

        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)

        end

    'YSTEP' : begin
        (*pState).rvolume->GetProperty, RENDER_STEP=render_step
        render_step[1] = event.value + 1
        (*pState).rVolume->SetProperty, RENDER_STEP=render_step

        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)

        end

    'ZSTEP' : begin
        (*pState).rvolume->GetProperty, RENDER_STEP=render_step
        render_step[2] = event.value + 1
        (*pState).rVolume->SetProperty, RENDER_STEP=render_step

        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)

        end
;
;   Set the user-indicated render quality.
;
    'QUALITY' : begin
        (*pState).render_quality = event.value + 1

        d_volrendrDraw, *pState, $
            QUALITY=([0,(*pState).render_quality])((*pState).auto_render)

        end
;
;   Handle events that occur in the drawing area.
;
    'DRAW': begin
        if (event.type eq 4) then begin ; Expose.
            d_volrendrRefresh, *pState
            endif

        if (event.type eq 0) then $
            if (event.press eq 1) AND ((*pState).lmb_scale eq 1) then $
                event.press = 2     ; virtual button 2 event.
;
;       Rotation updates.
;
        if (*pState).rModelArray[0]->Update(event) $
        or (*pState).rModelArray[1]->Update(event) then begin
            d_volrendrDraw, *pState, QUALITY=0
            end
;
;       Mouse button press.
;
        if (event.type eq 0) then begin
            case event.press of
                2 : begin
;
;                   Middle mouse-button.  Scale the objects.
;
                    xy = ([event.x, event.y] - (*pState).center)
                    r= TOTAL(xy^2) ; distance from center of unit circle
                    (*pState).sc[1] = SQRT(r)
                    (*pState).sc[2] = 1.0
                    end
                4 : begin
;
;                   Right mouse-button
;
                    (*pState).rWindow->GetProperty, QUALITY=current_quality
                    if (current_quality ge 1) then begin
;
;                       Pick a voxel point.
;
                        j = (*pState).rvolume->pickvoxel( $
                            (*pState).rWindow, $
                            (*pState).rView,[event.x, event.y] $
                            )
                        k = -1

                        if (j[0] NE -1) then begin
                            (*pState).rvolume->GetProperty, $
                                    DATA0=dd, /NO_COPY
                            k = dd[j[0],j[1],j[2]]
                            (*pState).rvolume->SetProperty, $
                                        DATA0=dd, /NO_COPY
                            end
;
;                       Display the point coordinates and its value.
;
                        str = string(   $
                            j[0],       $
                            j[1],       $
                            j[2],       $
                            k,          $
                            FORMAT='("X=",I3.3,",Y=",I3.3,",Z=",I3.3,' $
                                  +'",Value=",I3.3)' $
                            )
                        demo_putTips, (*pState), str, 10
                        end
                    end
                else:
                endcase
            (*pState).btndown = event.press
            WIDGET_CONTROL,(*pState).wDraw, /DRAW_MOTION
            endif
;
;       Mouse-button motion.
;
        if event.type eq 2 then begin
            case (*pState).btndown of
                4: begin ; Right mouse-button.
                    (*pState).rWindow->GetProperty, QUALITY=current_quality
                    if current_quality ge 1 then begin
                        j = (*pState).rvolume->pickvoxel( $
                                (*pState).rWindow, $
                                (*pState).rView,[event.x,event.y] $
                                )
                        k= -1

                        if (j[0] NE -1) then begin
                            (*pState).rvolume->GetProperty, $
                                    DATA0=dd, /NO_COPY
                            k = dd[j[0],j[1],j[2]]
                            (*pState).rvolume->SetProperty, $
                                    DATA0=dd, /NO_COPY
                            end
;
;                       Display the voxel location and value.
;
                        str = string(   $
                            j[0],       $
                            j[1],       $
                            j[2],       $
                            k,          $
                            FORMAT='("X=",I3.3,",Y=",I3.3,",Z=",I3.3,' $
                                  +'",Value=",I3.3)' $
                            )
                        demo_putTips, (*pState), str, 10
                        end
                    end
                2: begin
                    xy = ([event.x,event.y] - (*pState).center)
                    r = total(xy^2) ; distance from center of unit circle
                    (*pState).sc[2] = (SQRT(r) $
                                    / (*pState).sc[1]) $
                                    / (*pState).sc[2]
                    (*pState).rScaleToys->Scale, $
                        (*pState).sc[2], $
                        (*pState).sc[2], $
                        (*pState).sc[2]
                    (*pState).rScaleVolumes->Scale, $
                        (*pState).sc[2], $
                        (*pState).sc[2], $
                        (*pState).sc[2]
                    (*pState).rModelArray[0]->GetProperty, $
                        RADIUS=radius
                    (*pState).rModelArray[0]->SetProperty, $
                        RADIUS=radius*(*pState).sc[2]
                    (*pState).rModelArray[1]->SetProperty, $
                        RADIUS=radius*(*pState).sc[2]
                    (*pState).sc[2] = (SQRT(r)/(*pState).sc[1])
                    d_volrendrDraw, *pState, QUALITY=0
                    end
                else:
                endcase
            end
;
;       Mouse-button release.
;
        if (event.type eq 1) then begin
            case (*pState).btndown of
                2: begin
                    (*pState).sc[0] = (*pState).sc[2] * (*pState).sc[0]
                    end
                4: begin
                    (*pState).rWindow->GetProperty, QUALITY=current_quality
                    if current_quality ge 1 then begin
                        j = (*pState).rvolume->pickvoxel( $
                            (*pState).rWindow, $
                            (*pState).rView,[event.x,event.y] $
                            )
                        k = -1

                        if (j[0] NE -1) then begin
                            (*pState).rvolume->GetProperty, $
                                    DATA0=dd, /NO_COPY
                            k = dd[j[0],j[1],j[2]]
                            (*pState).rvolume->SetProperty, $
                                    DATA0=dd, /NO_COPY
;
;                           Get the volume's coordinate transform.
;
                            (*pState).rvolume->GetProperty, $
                                        XCOORD_CONV=x_conv, $
                                        YCOORD_CONV=y_conv, $
                                        ZCOORD_CONV=z_conv
;
;                           Convert to normal coordinates.
;
                            jack = FLTARR(3)
                            jack[0] = x_conv[1]*j[0] + x_conv[0]
                            jack[1] = y_conv[1]*j[1] + y_conv[0]
                            jack[2] = z_conv[1]*j[2] + z_conv[0]
;
;                           Apply the difference.
;
                            (*pState).rOtherObjectArray[6]->Translate, $
                                jack[0]-(*pState).jpos[0], $
                                jack[1]-(*pState).jpos[1], $
                                jack[2]-(*pState).jpos[2]
;
;                           Store the new location.
;
                            (*pState).jpos = jack
;
                            (*pState).cursor_stale = 1b ; Until it is drawn.
                            (*pState).rOtherObjectArray[2]->GetProperty, $
                                HIDE=hide
                            if (hide eq 0) and ((*pState).auto_render eq 1) $
                            then $
                                d_volrendrDraw, *pState, $
                                    QUALITY=(*pState).render_quality
                            end
;
;                       Display the voxel location and value numerically.
;
                        str = string(   $
                            j[0],       $
                            j[1],       $
                            j[2],       $
                            k,          $
                            FORMAT='("X=",I3.3,",Y=",I3.3,",Z=",I3.3,' $
                                  +'",Value=",I3.3)' $
                            )
                        demo_putTips, (*pState), str, 10
                        end
                    end
                else:
                endcase

            if (*pState).auto_render then begin
                (*pState).rWindow->GetProperty, QUALITY=current_quality
                if current_quality ne (*pState).render_quality then $
                    d_volrendrDraw, *pState, QUALITY=(*pState).render_quality
                end

            (*pState).btndown = 0
            WIDGET_CONTROL, (*pState).wDraw, DRAW_MOTION=0

            endif

        end   ;of DRAW
;
;   Quit this application.
;
    'QUIT' : begin
        WIDGET_CONTROL, event.top, /DESTROY
        RETURN
        end   ; of QUIT

    'HELP' : begin
;
        RETURN
        end   ; of HELP

    else:
    endcase

end
;
;----------------------------------------------------------------------------
;
;  Purpose:  Destroy the top objects and restore the previous
;         color table.
;
pro d_volrendrCleanup, $
    wTopBase        ;  IN: top level base identifier

WIDGET_CONTROL, wTopBase, GET_UVALUE=pState
;
;Restore the color table.
;
TVLCT, (*pState).colorTable

if WIDGET_INFO((*pState).groupBase, /VALID) then $
    WIDGET_CONTROL, (*pState).groupBase, /MAP
;
;Clean up heap variables.
;
for i=0,n_tags(*pState)-1 do begin
    case size((*pState).(i), /TNAME) of
        'POINTER': $
            ptr_free, (*pState).(i)
        'OBJREF': $
            obj_destroy, (*pState).(i)
        else:
        endcase
    end
PTR_FREE, pState

end
;
;----------------------------------------------------------------------------
;
;  Purpose:  Perform volume rendering.
;
pro volume_render, volume, GROUP=group
;
;Check the validity of the group identifier
;
ngroup = N_ELEMENTS(group)
if (ngroup NE 0) then begin
    check = WIDGET_INFO(group, /VALID_ID)
    if (check NE 1) then begin
        print,'Error, the group identifier is not valid'
        print, 'Returning to the main application'
        RETURN
        endif
    groupBase = group
    endif $
else $
    groupBase = 0L
;
; Make sure volume is a 3-D, byte array
size = size(volume, /structure)
if (size.n_dimensions ne 3) or (size.type_name ne 'BYTE') then begin
    message, 'Input parameter must be a 3-D byte array'
endif

;
;Get the current color vectors to restore
;when this application is exited.
;
TVLCT, savedR, savedG, savedB, /GET
;
;Build color table from color vectors
;
colorTable = [[savedR],[savedG],[savedB]]

;
;Create the PLEASE WAIT text.
;
rFont = OBJ_NEW('IDLgrFont', 'Helvetica', SIZE=18)
rText = OBJ_NEW('IDLgrText', $
    'Starting up  Please wait...', $
    ALIGN=0.5, $
    LOCATION=textLocation, $
    COLOR=[255,255,0], FONT=rFont)
;
;Set up dimensions for the drawing (viewing) area.
;
device, GET_SCREEN_SIZE=scr
xdim = scr(0) * 0.6; * 0.85
ydim = xdim   * 0.8; * 0.85
;
;Create  model tree.
;
rModelArray = OBJARR(4)
rModelArray[3] = OBJ_NEW('IDLgrModel')
rModelArray[0] = OBJ_NEW('IDLexRotator', $
    [xdim/2.0, ydim/2.0], $
    xdim/2.0 $
    )
rModelArray[1] = OBJ_NEW('IDLexRotator', $
    [xdim/2.0, ydim/2.0], $
    xdim/2.0 $
    )
rModelArray[3]->Add, rModelArray[1]
rModelArray[2] = OBJ_NEW('IDLgrModel')
rModelArray[3]->Add, rModelArray[2]
;
;Add the top model to the view.
;
rView = OBJ_NEW('IDLgrView')
rView->Add, rModelArray[3]
;
rModelArray[3]->Add, rText
;
;Introduce a graphics tree nodes dedicated to scaling.
;
rScaleToys = obj_new('IDLgrModel')
rScaleVolumes = obj_new('IDLgrModel')
rModelArray[0]->Add, rScaleToys
;
;Load up the volumes.
;

WIDGET_CONTROL, /HOURGLASS
;Construct volume object, applying translation and scale such that the volume
;fits inside a 1-unit cube centered at 0,0,0.
;
i = SIZE(volume)
zc = 1.0
m = i[1] > i[2] > (i[3] * zc)
nx = i[1]
ny = i[2]
nz = i[3]
sx = 1.0 / m            ; scale x.
sy = 1.0 / m            ; scale y.
sz =(1.0 / m) * zc      ; scale z.
ox = -i[1] * sx * 0.5   ; offset x.
oy = -i[2] * sy * 0.5   ; offset y.
oz = -i[3] * sz * 0.5   ; offset z.

rVolume = OBJ_NEW('IDLgrVolume', $
    volume, $
    xcoord_conv=[ox, sx], $
    ycoord_conv=[oy, sy], $
    zcoord_conv=[oz, sz], $
    /NO_COPY, $
    NAME=volume_name)

rVolume->SetProperty, HINT=2 ; Use multiple CPUs, if we can.

rVolume->SetProperty, /ZERO_OPACITY_SKIP, HIDE=1
rVolume->SetProperty, /ZBUFFER, VOLUME_SELECT=volume_select

rScaleVolumes->Add, rVolume

rModelArray[1]->Add, rScaleVolumes
;
;Rotate for a nice initial view...
;
rModelArray[1]->Rotate,[0,0,1],-45
rModelArray[0]->Rotate,[0,0,1],-45
rModelArray[1]->Rotate,[1,0,0],-75
rModelArray[0]->Rotate,[1,0,0],-75
;
;Create other intermixed objects.
;
rOtherObjectArray = OBJARR(8)
;
;Create a wire box.
;
xp=[-1, 1, 1,-1, $
    -1, 1, 1,-1] * .5
yp=[-1,-1, 1, 1, $
    -1,-1, 1, 1] * .5
zp=[ 1, 1, 1, 1, $
    -1,-1,-1,-1] * .5
pl=[5,0,1,2,3,0, $
    5,4,5,6,7,4, $
    2,0,4, $
    2,1,5, $
    2,2,6, $
    2,3,7]

rOtherObjectArray[0] = OBJ_NEW('IDLgrPolyline', xp, yp, zp, $
    POLYLINES=pl, COLOR=[255,255,255])

rScaleToys->Add, rOtherObjectArray[0]
;
;Create a solid plane.
;
o = 0.3
verts = TRANSPOSE([[-o,o,o,-o],[-o,-o,o,o],[-o,-o,o,o]])
poly = [4,0,1,2,3]
vc = [200B, 200B, 200]
vc = [[vc],[vc],[vc],[vc]]

rOtherObjectArray[1] = OBJ_NEW('IDLgrPolygon', verts, POLYGONS=poly, $
    VERT_COLOR=vc, SHADING=1)

rScaleToys->Add, rOtherObjectArray[1]
;
;Create 3D Cursor.
;
xpc = [-1.,1.,0.,0.,0.,0.] * .5
ypc = [0.,0.,-1.,1.,0.,0.] * .5
zpc = [0.,0.,0.,0.,-1.,1.] * .5
plc = [2,0,1,2,2,3,2,4,5]

rOtherObjectArray[2] = OBJ_NEW('IDLgrPolyline', xpc, ypc, zpc, $
    POLYLINES=plc, COLOR=[255,255,128])
;
;Something to move the 3D Cursor with.
;
rOtherObjectArray[6] = OBJ_NEW('IDLgrModel')
rOtherObjectArray[6]->Add,  rOtherObjectArray[2]
rScaleToys->Add,  rOtherObjectArray[6]
;
;Create Axes.
;
xpc = [-1.,1.,0.,0.,0.,0.]
ypc = [0.,0.,-1.,1.,0.,0.]
zpc = [0.,0.,0.,0.,-1.,1.]
plc = [2,0,1,2,2,3,2,4,5]
vcc = [[255B,0B,0B],[255B,0B,0B], $
       [0B,255B,0B],[0B,255B,0B], $
       [0B,0B,255B],[0B,0B,255B] $
      ]

rOtherObjectArray[7] = OBJ_NEW('IDLgrPolyline', xpc, ypc, zpc, $
    POLYLINES=plc, VERT_COLOR=vcc)

rScaleToys->Add,  rOtherObjectArray[7]
;
;Create a text for information on the objects.
;
font24 = OBJ_NEW( 'IDLgrFont', 'Helvetica', size=18. )

rOtherObjectArray[5] = OBJ_NEW( 'IDLgrText', LOCATION=[10,10], $
    'Electron Probability Density 64x64x64', COLOR=[255,255,0], $
    FONT=font24,HIDE=1)

rModelArray[2]->Add, rOtherObjectArray[5]
;
;Set to thick lines.
;
if (N_elements(thick) NE 0) then begin
    rOtherObjectArray[2]->SetProperty, THICK=2.0
    rOtherObjectArray[0]->SetProperty, THICK=2.0
    end
;
;Hide the other objects to start.
;
rOtherObjectArray[0]->SetProperty, HIDE=1
rOtherObjectArray[1]->SetProperty, HIDE=1
rOtherObjectArray[2]->SetProperty, HIDE=1
;
;Create s lights.
;
vl = OBJ_NEW('IDLgrLight', DIRECTION=[-1,0,1], TYPE=2)
vl->SetProperty, COLOR=[255,255,255], INTENSITY=1.0
rModelArray[3]->Add, vl

sl = OBJ_NEW('IDLgrLight', TYPE=0, INTENSITY=1.0)
rModelArray[3]->Add, sl
;
;Enable the volume
;
rvolume->SetProperty, HIDE=0
;
;Set the axes to not show.
;
jack = FLTARR(3)
jack = [0.0,0.0,0.0]
;
;Set up dimensions for the drawing (viewing) area.
;
device, GET_SCREEN_SIZE=scr
xdim = scr(0) * 0.6; * 0.85
ydim = xdim   * 0.8; * 0.85
;
;Create widgets.
;
if (N_ELEMENTS(group) eq 0) then begin
    wBase = WIDGET_BASE(/COLUMN, XPAD=0, YPAD=0, $
        TITLE="Volumes", $
        /TLB_KILL_REQUEST_EVENTS, $
        UNAME='d_volrendr:tlb', $
        TLB_FRAME_ATTR=1, MBAR=barBase)
    endif else begin
    wBase = WIDGET_BASE(/column, XPAD=0, YPAD=0, $
        TITLE="Volumes", $
        GROUP_LEADER=group, $
        /TLB_KILL_REQUEST_EVENTS, $
        UNAME='d_volrendr:tlb', $
        TLB_FRAME_ATTR=1, MBAR=barBase)
    endelse
;
;Create the menu bar.
;
fileMenu = WIDGET_BUTTON(barBase, VALUE='File', /MENU)

wQuitButton = WIDGET_BUTTON(fileMenu, VALUE='Quit', UVALUE='QUIT', $
    UNAME='d_volrendr:quit')

wOptionButton = WIDGET_BUTTON(barBase, VALUE='Options', /MENU)

    wWireBoxButton = WIDGET_BUTTON(wOptionButton, $
        VALUE="Wire Box (off)", UVALUE='WIREBOX', $
        UNAME='d_volrendr:wirebox')

    wAxesButton = WIDGET_BUTTON(wOptionButton, $
        VALUE='Axis Lines (off)', UVALUE='AXES', $
        UNAME='d_volrendr:axes')

    wCursorButton = WIDGET_BUTTON(wOptionButton, $
        VALUE='3D Cursor (off)', UVALUE='CURSOR', $
        UNAME='d_volrendr:cursor')

wColorButton = WIDGET_BUTTON(barBase, VALUE='Colors', /MENU)

    wRGBButton = WIDGET_BUTTON(wColorButton, $
        VALUE="RGB", UVALUE='RGB', $
        UNAME='d_volrendr:rgb')

    wOpacityButton = WIDGET_BUTTON(wColorButton, $
        VALUE='Opacity', UVALUE='OPACITY', $
        UNAME='d_volrendr:opacity')

;
;
;Create the menu bar item help that contains the about button
;
hlp = WIDGET_BUTTON(barBase, VALUE='Help', /HELP, /MENU)

    wHelpButton = WIDGET_BUTTON(hlp, $
        VALUE='Help', UVALUE='HELP')
;
subBase = WIDGET_BASE(wBase, COLUMN=2)

    wLeftbase = WIDGET_BASE(subBase, /COLUMN)
        wRenderButton = WIDGET_BUTTON(wLeftBase, $
            VALUE="Render", UVALUE='RENDER', UNAME='d_volrendr:render')
    	wPNGButton = WIDGET_BUTTON(wLeftBase, $
            VALUE="Generate PNG", UVALUE='PNG', UNAME='d_volrendr:png')

        wNonExclusiveBase = WIDGET_BASE(wLeftBase, /NONEXCLUSIVE)
            wAutoRenderButton = WIDGET_BUTTON(wNonExclusiveBase, $
                VALUE="Auto-Render", UVALUE='AUTORENDER', $
                UNAME='d_volrendr:autorender')
            wLightButton = WIDGET_BUTTON(wNonExclusiveBase, $
                VALUE="Gradient Shading",  UVALUE='LIGHTING', $
                UNAME='d_volrendr:gradient_shade')

        wQuality = CW_BGROUP(wLeftBase, $
            ['medium (faster)', 'high'], $
            LABEL_TOP='Rendering Quality:', $
            /EXCLUSIVE, $
            UVALUE='QUALITY', $
            /NO_RELEASE, $
            /FRAME)
        WIDGET_CONTROL, wQuality, SET_UNAME='d_volrendr:quality_radio'

        void = WIDGET_LABEL(wLeftBase,  $
            VALUE='Rendering mode:', $
            /ALIGN_LEFT)
        wLMBMode = WIDGET_DROPLIST(wLeftBase, $ ;
            VALUE=["Alpha blending", $
                   "Maximum intensity", $
                   "Alpha sum", $
                   "Average"], $
            UVALUE='RENDER_MODE', $
            UNAME='d_volrendr:render_mode', $
            /ALIGN_LEFT)

        void = WIDGET_LABEL(wLeftBase,  $
            VALUE='Left Mouse-Button Action:', $
            /ALIGN_LEFT)
        wLMBMode = WIDGET_DROPLIST(wLeftBase, $ ; left mouse-button mode
            VALUE=["Rotate Unconstrained", $
                "Rotate about Screen X", $
                "Rotate about Screen Y", $
                "Rotate about Screen Z", $
                "Rotate about Data X (Red)", $
                "Rotate about Data Y (Green)", $
                "Rotate about Data Z (Blue)", $
                "Scale"], $
            UVALUE='LMBMODE', $
            UNAME='d_volrendr:mouse_mode', $
            /ALIGN_LEFT)

        wXMin = WIDGET_SLIDER(wLeftBase, $
            TITLE='X Min cutting plane (%)', $
            UVALUE='XMIN', $
            VALUE=0, $
            UNAME='d_volrendr:xmin')

        wXMax = WIDGET_SLIDER(wLeftBase, $
            TITLE='X Max cutting plane (%)', $
            UVALUE='XMAX', $
            VALUE=100, $
            UNAME='d_volrendr:xmax')

        wYMin = WIDGET_SLIDER(wLeftBase, $
            TITLE='Y Min cutting plane (%)', $
            UVALUE='YMIN', $
            VALUE=0, $
            UNAME='d_volrendr:ymin')

        wYMax = WIDGET_SLIDER(wLeftBase, $
            TITLE='Y Max cutting plane (%)', $
            UVALUE='YMAX', $
            VALUE=100, $
            UNAME='d_volrendr:ymax')

        wZMin = WIDGET_SLIDER(wLeftBase, $
            TITLE='Z Min cutting plane (%)', $
            UVALUE='ZMIN', $
            VALUE=0, $
            UNAME='d_volrendr:zmin')

        wZMax = WIDGET_SLIDER(wLeftBase, $
            TITLE='Z Max cutting plane (%)', $
            UVALUE='ZMAX', $
            VALUE=100, $
            UNAME='d_volrendr:zmax')

    wRightbase = WIDGET_BASE(subBase)
;
;       Use IDL's software renderer because it is fast at
;       rendering volumes.
;
        renderer=1
;
        wDraw = WIDGET_DRAW(wRightBase, $
            GRAPHICS_LEVEL=2,   $
            XSIZE=xdim,         $
            YSIZE=ydim,         $
            /BUTTON_EVENTS,     $
            UVALUE='DRAW',      $
            RETAIN=0,           $
            /EXPOSE_EVENTS,     $
            UNAME='d_volrendr:draw', $
            RENDERER=renderer)
        wHotKeyReceptor = WIDGET_TEXT(wRightBase, $
            /ALL_EVENTS, $
            UVALUE='HOTKEY', $
            UNAME='d_volrendr:hotkey')
;
;Create the status line label.
;
wStatusBase = WIDGET_BASE(wBase, MAP=0, /ROW)
;
WIDGET_CONTROL, wBase, /REALIZE
WIDGET_CONTROL, /HOURGLASS
appTLB = wBase ; Returns the top level base to the APPTLB keyword.
sText = demo_getTips(demo_filepath('volrendr.tip', $
                     SUBDIR=['examples','demo', 'demotext']), $
                     wBase, $
                     wStatusBase)

WIDGET_CONTROL, wBase, /CLEAR_EVENTS
WIDGET_CONTROL, wBase, SENSITIVE=0
;WIDGET_CONTROL, wStartMes, /SHOW
;
;Grab a refernce to the drawable.
;
WIDGET_CONTROL, wDraw, GET_VALUE=rWindow
;
;Compute viewplane rectangle to nicely fit our volumes.
;
zoom = sqrt(2) ; Nicety. (Length of unit-cube face diagonal.)
myview = [-.5, -.5, 1, 1] * zoom
;
;Grow viewplane rectangle to match wDraw's aspect ratio.
;
aspect = FLOAT(xdim)/FLOAT(ydim)
if (aspect gt 1) then begin
    myview[0] = myview[0] - ((aspect-1.0)*myview[2])/2.0
    myview[2] = myview[2] * aspect
    end $
else begin
    myview[1] = myview[1] - (((1.0/aspect)-1.0)*myview[3])/2.0
    myview[3] = myview[3] / aspect
    end
rView->SetProperty,         $
    VIEWPLANE_RECT=myview,  $
    PROJECTION=1,           $
    EYE=2.0,                $
    ZCLIP=[1.0,-1.0],       $
    COLOR=[0,0,0]
;
;Create a transparent view that is just like our established view.
;(Used for screen refreshes.)
;
rTransparentView = OBJ_NEW('IDLgrView', $
    VIEWPLANE_RECT=myview,  $
    PROJECTION=1,           $
    EYE=2.0,                $
    ZCLIP=[1.0,-1.0],       $
    COLOR=[0,0,0],          $
    /TRANSPARENT            $
    )
;
;Save the state of our application.
;
pState = PTR_NEW({      $
    center: [xdim, ydim] / 2., $ ; Center of drawing area.
    radius: ydim / 2,   $ ; Sphere radius (1/2 of draw area height)
    axis: 3b,           $ ; constrict Trackball rotations. 0=x,1=y,2=z,3=no.
    btndown: 0b,        $ ; which mouse button is pressed.
    pt0: FLTARR(3),     $ ; Position point 0
    pt1: FLTARR(3),     $ ; Position point 1
    sc: FLTARR(3),      $ ; Scaling factor for x, y, z directions
    wDraw: wDraw,       $ ; Widget draw ID
    wHotKeyReceptor: wHotKeyReceptor, $
    wLMBMode: wLMBMode, $
    wRenderButton: wRenderButton, $
    wPNGButton: wPNGButton, $
    wAutoRenderButton: wAutoRenderButton, $
    wLightButton: wLightButton, $
    wWireBoxButton: wWireBoxButton, $
    wAxesButton: wAxesButton, $
    wCursorButton: wCursorButton, $
    rModelArray: rModelArray, $ ; Model array
    cur: 0,             $ ; Current object shown (0=brain, 1=cloud)
    rVolume: rVolume, $ ; Volume object reference
    rOtherObjectArray: rOtherObjectArray, $ ; Other object references
    rScaleToys: rScaleToys, $
    rScaleVolumes: rScaleVolumes, $
    rView: rView,       $ ; View object reference
    font24: font24,     $
    rWindow: rWindow,   $ ; Window object reference
    rTransparentView: rTransparentView, $
    rToysModel: rModelArray[0], $
    sText: sText,       $ ; Text structure for tips
    jpos: jack,         $ ; Axes position
    ColorTable: colorTable, $ ; Color table to restore
    rText: rText,       $ ; Text object refence
    rFont: rFont,       $ ; Font object refence
    wBase: wBase,       $ ; top level base
    wXMin: wXMin, $
    wXMax: wXMax, $
    wYMin: wYMin, $
    wYMax: wYMax, $
    wZMin: wZMin, $
    wZMax: wZMax, $
    lmb_scale: 0,        $ ; Left mouse button scaling mode: 0=not on, 1=on
    suppress_draws: 1b, $ ; 1=yes, 0=no
    auto_render:0,      $ ; 1=yes, 0=no
    screen_rotate:1b,   $ ; 1=rotations are with respect to screen axes.
    an_instance_exists: 0b, $
    render_quality: 1,  $ ; 1 ('medium') or 2 ('high')
    cursor_stale: 0B, $   ; 3D cursor up to date in graphic?
    wQuality: wQuality, $
    groupBase: groupBase $; Base of Group Leader
    })

WIDGET_CONTROL, wBase, SET_UVALUE=pState
;
rModelArray[3]->Remove, rText
;
;Manually send a sequence of clicks to initialize the application.
;
WIDGET_CONTROL, wQuality, SET_VALUE=0
event = {           $
    id: wQuality,   $
    top: wBase,     $
    handler: 0L,    $
    select: 1,      $
    value: 0        $
    }
d_volrendrEvent, event


; Initialize cutting planes here
cutting_plane = [[1,0,0,0], $
                 [-1,0,0,nx], $
                 [0,1,0,0], $
                 [0,-1,0,ny], $
                 [0,0,1,0], $
                 [0,0,-1,nz]]
rVolume->SetProperty, CUTTING_PLANE=cutting_plane

d_volrendrEvent, { $
    id: wAxesButton, $
    top: wBase, $
    handler: 0L $
    }

(*pState).suppress_draws = 0

event.id = wRenderButton
d_volrendrEvent, event
;
;Now we are ready to handle user events.
;
;WIDGET_CONTROL, wStartMes, /DESTROY
XMANAGER, 'd_volrendr', wBase,       $
    Event_Handler='d_volrendrEvent', $
    CLEANUP='d_volrendrCleanup',     $
    /NO_BLOCK

WIDGET_CONTROL, wBase, SENSITIVE=1
end   ;  of d_volrendr.pro
