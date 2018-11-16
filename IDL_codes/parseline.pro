pro parseline,A$,sep$,S$,numstr
;
; This procedure parses an input string into an array containing delimited
;  substrings
;
; A$      input string to be parsed
; sep$    a single character - substring separator
; S$      output array of parsed substrings
; numstr  number of output parsed substrings
;
alength=STRLEN(A$)
alengthp1=LONG(alength)+1
v=intarr(alengthp1)            ; v array 0 if separator / 1 if non separator
stnum=intarr(alengthp1)      ; stnum array starting character of each substring
endnum=intarr(alengthp1)  ; stnum array starting character of each substring
b=BYTE(A$)
sepb=BYTE(sep$)
nm= 0
mm=-1

havesep=WHERE(b EQ sepb(0))
if(havesep(0) EQ -1L) then BEGIN
   S$=STRARR(1)
   S$(0)=A$
  numstr=1
   RETURN
ENDIF
v(WHERE(b EQ sepb(0)))=0
  v(WHERE(b NE sepb(0)))=1

FOR j=0,alength DO BEGIN

  if(v(j) EQ 1) then BEGIN

    if(nm EQ 0) then BEGIN

       nm=1
       mm=mm+1
       stnum(mm)=j
       ; print,'stnum(',mm,')=',stnum(mm)
    endif
  endif

  if(v(j) EQ 0) then BEGIN
    
    if(nm EQ 1) then BEGIN

       nm=0
       endnum(mm)=j-1
       ;print,'endnum(',mm,')=',endnum(mm)
    endif
  endif
endfor


numstr=mm+1

 S$=strarr(numstr)

   for j=0,numstr-1 DO BEGIN

       S$(j)=STRMID(A$,stnum(j),(endnum(j)-stnum(j)+1))
       ;print,'S$(',j,')=',S$(j)
   endfor
END
