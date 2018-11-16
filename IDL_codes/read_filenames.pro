pro  read_filenames,filelist$,files$,numfiles


; This IDL procedure ingests the specified ascii filelist file and
; returns a string array containing the files and the number of files
;
; filelist$ - character string containing the filelist filename
;               ex: '/Jukelists/npsmmronssmi1dtbascfilelist'
; files$ - string array returning the filenames in the filelist file
; numfiles - returning the number of files in the filelist
;
;                    Stephen Fiegles HSTX


;
i=0L
tempfile$=STRING(REPLICATE(32b,255))
files$=''
On_ioerror,badfile
openr,unit,filelist$,/get_lun
WHILE (NOT EOF(unit)) DO BEGIN
 readf,unit,tempfile$
   i=i+1L
 ENDWHILE
numfiles=i
;print,' numfiles=',numfiles
files$=strarr(numfiles)
POINT_LUN,unit,0

for k=0L,numfiles-1  DO BEGIN
 readf,unit,tempfile$
;print,' i=',k,' tempfile=',tempfile$
  files$(k)=STRTRIM(tempfile$,2)

 ENDFOR
FREE_LUN,unit

goto,readend
badfile:
print,' ERROR reading file: filelist'
print ,!ERR_STRING
files$=""
FREE_LUN,unit
READEND:
END

