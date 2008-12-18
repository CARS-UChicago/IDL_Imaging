pro make_epics_ad_display_sav
   ; This IDL procedure makes epics_ad_display.sav, to run under the IDL Virtual Machine
   filenames = ['epics_ad_display']
   classnames = ['epics_ad_display', 'image_display']
   resolve_routine, filenames, /either, /compile_full_file, /no_recompile
   resolve_all, class=classnames
   itresolve
   save, /routine, file='epics_ad_display.sav'
end

