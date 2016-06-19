pro test_write_video, data, file=file, min=min, max=max, bit_rate=bit_rate

if (n_elements(min) eq 0) then min = min(data)
if (n_elements(max) eq 0) then max = max(data)
s = size(data, /dimensions)
nx = s[0]
ny = s[1]
nz = s[2]
if (n_elements(file) eq 0) then file = 'C:\temp\test.mp4'

frame = bytarr(3, nx, ny)
for i=0, nz-1 do begin
  temp = bytscl(data[*,*,i], min=min, max=max)
  frame[0,*,*] = temp
  frame[1,*,*] = temp
  frame[2,*,*] = temp
  print, 'writing frame ', i
  write_video, file, frame, handle=handle, video_fps=30, bit_rate=bit_rate
endfor

write_video, file, handle=handle, /close
end
