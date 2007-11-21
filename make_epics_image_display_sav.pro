pro make_epics_image_display_sav
   ; This IDL procedure makes epics_image_display.sav, to run under the IDL Virtual Machine
   filenames = ['epics_image_display']
   classnames = ['epics_image_display', 'image_display']
   resolve_routine, filenames, /either, /compile_full_file, /no_recompile
   resolve_all, class=classnames
   itresolve
   save, /routine, file='epics_image_display.sav'
end

