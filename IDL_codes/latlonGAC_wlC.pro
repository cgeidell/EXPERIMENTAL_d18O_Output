pro latlonsub_nolbl,ltmn,ltmx,ltinc,lnmn,lnmx,lninc,pole,nc,nr,mag,black_c,lbl$

	PLOT,[0,1],[0,1],XRANGE=[0,nc*mag],XSTYLE=5, $
	  YRANGE=[nr*mag,0],YSTYLE=5,/NODATA,/noerase, $
	  position=[0,0,1,1]

	latmin=double(ltmn)
	latmax=double(ltmx)
	latinc=double(ltinc)
	lonmin=double(lnmn)
	lonmax=double(lnmx)
	loninc=double(lninc)

	arrsize=FIX(((lonmax-lonmin)/loninc)+1+1)
	rcoll=dblarr(arrsize)
	rroww=dblarr(arrsize)
	rlat=dblarr(arrsize)
	rlon=dblarr(arrsize)
	i=0.0D
	j=0.0D

	FOR i=latmin,latmax,latinc DO BEGIN
	  nn=0
	  FOR j=lonmin,lonmax,loninc DO BEGIN
     	rlat(nn)=DOUBLE(i)
     	rlon(nn)=DOUBLE(j)
     	nn=nn+1
	  ENDFOR
	  ssmilltoijarr,rlat,rlon,rrowidl,rcolidl,pole,'W',85
	  rcoll=rcolidl(0:arrsize-3)
	  rroww=rrowidl(0:arrsize-3)
	  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = 2
	ENDFOR

	arrsize2=FIX(((latmax-latmin)/latinc)+1+1)
	rcoll=fltarr(arrsize2)
	rroww=fltarr(arrsize2)
	i=0.0D
	j=0.0D
	loninc=45
	ip = 0
	FOR j=lonmin,lonmax,loninc DO BEGIN
	  nn=0
	  FOR i=latmin,latmax,latinc DO BEGIN
	    rlat(nn)=DOUBLE(i)
	    rlon(nn)=DOUBLE(j)
	    nn=nn+1
	  ENDFOR
	  ssmilltoijarr,rlat,rlon,rrowidl,rcolidl,pole,'W',85
	  rcoll=rcolidl(0:arrsize2-3)
	  rroww=rrowidl(0:arrsize2-3)
	  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = 2
	;
	ip=ip+1
	ENDFOR
END
pro latlonsub_xlblthick,ltmn,ltmx,ltinc,lnmn,lnmx,lninc,pole,nc,nr,mag,black_c,lbl$, thickin

	PLOT,[0,1],[0,1],XRANGE=[0,nc*mag],XSTYLE=5, $
	  YRANGE=[nr*mag,0],YSTYLE=5,/NODATA,/noerase, $
	  position=[0,0,1,1]

	latmin=double(ltmn)
	latmax=double(ltmx)
	latinc=double(ltinc)
	lonmin=double(lnmn)
	lonmax=double(lnmx)
	loninc=double(lninc)

	arrsize=FIX(((lonmax-lonmin)/loninc)+1+1)
	rcoll=dblarr(arrsize)
	rroww=dblarr(arrsize)
	rlat=dblarr(arrsize)
	rlon=dblarr(arrsize)
	i=0.0D
	j=0.0D

	FOR i=latmin,latmax,latinc DO BEGIN
	  nn=0
	  FOR j=lonmin,lonmax,loninc DO BEGIN
     	rlat(nn)=DOUBLE(i)
     	rlon(nn)=DOUBLE(j)
     	nn=nn+1
	  ENDFOR
	  ssmilltoijarr,rlat,rlon,rrowidl,rcolidl,pole,'W',85
	  rcoll=rcolidl(0:arrsize-3)
	  rroww=rrowidl(0:arrsize-3)
	  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = thickin
	ENDFOR

	arrsize2=FIX(((latmax-latmin)/latinc)+1+1)
	rcoll=fltarr(arrsize2)
	rroww=fltarr(arrsize2)
	i=0.0D
	j=0.0D
	loninc=45
	ip = 0
	FOR j=lonmin,lonmax,loninc DO BEGIN
	  nn=0
	  FOR i=latmin,latmax,latinc DO BEGIN
	    rlat(nn)=DOUBLE(i)
	    rlon(nn)=DOUBLE(j)
	    nn=nn+1
	  ENDFOR
	  ssmilltoijarr,rlat,rlon,rrowidl,rcolidl,pole,'W',85
	  rcoll=rcolidl(0:arrsize2-3)
	  rroww=rrowidl(0:arrsize2-3)
	  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = thickin
	;
	ip=ip+1
	ENDFOR
END

pro latlonsub_wl,ltmn,ltmx,ltinc,lnmn,lnmx,lninc,pole,nc,nr,mag,black_c,lbl$

PLOT,[0,1],[0,1],XRANGE=[0,nc*mag],XSTYLE=5, $
  YRANGE=[nr*mag,0],YSTYLE=5,/NODATA,/noerase, $
  position=[0,0,1,1]

latmin=double(ltmn)
latmax=double(ltmx)
latinc=double(ltinc)
lonmin=double(lnmn)
lonmax=double(lnmx)
loninc=double(lninc)

arrsize=FIX(((lonmax-lonmin)/loninc)+1+1)
rcoll=dblarr(arrsize)
rroww=dblarr(arrsize)
rlat=dblarr(arrsize)
rlon=dblarr(arrsize)
i=0.0D
j=0.0D

FOR i=latmin,latmax,latinc DO BEGIN
  nn=0
  FOR j=lonmin,lonmax,loninc DO BEGIN
     rlat(nn)=DOUBLE(i)
     rlon(nn)=DOUBLE(j)
     nn=nn+1
  ENDFOR
  ssmilltoijarr,rlat,rlon,rrowidl,rcolidl,pole,'W',85
  rcoll=rcolidl(0:arrsize-3)
  rroww=rrowidl(0:arrsize-3)
  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = 2
ENDFOR

arrsize2=FIX(((latmax-latmin)/latinc)+1+1)
rcoll=fltarr(arrsize2)
rroww=fltarr(arrsize2)
i=0.0D
j=0.0D
loninc=45
ip = 0
FOR j=lonmin,lonmax,loninc DO BEGIN
  nn=0
  FOR i=latmin,latmax,latinc DO BEGIN
    rlat(nn)=DOUBLE(i)
    rlon(nn)=DOUBLE(j)
    nn=nn+1
  ENDFOR
  ssmilltoijarr,rlat,rlon,rrowidl,rcolidl,pole,'W',85
  rcoll=rcolidl(0:arrsize2-3)
  rroww=rrowidl(0:arrsize2-3)
  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = 2
;
ip=ip+1
ENDFOR
    IF (pole EQ 1) THEN polechar = '!5N' ELSE polechar = '!5S'
IF (pole EQ 1) THEN BEGIN
	SSMILLTOIJARR, 50., 135., rrow4xy, rcol4xy,pole,'E',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!550!9%'+polechar, /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = 2.

	SSMILLTOIJARR, 60., 135., rrow4xy, rcol4xy,pole,'E',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!560!9%'+polechar, /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = 2.

	SSMILLTOIJARR, 60., 180., rrow4xy, rcol4xy,pole,'W',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!5180!9%!5E', /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = 2.

	SSMILLTOIJARR, 60., 90., rrow4xy, rcol4xy,pole,'E',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!590!9%!5E', /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = 2.
ENDIF ELSE BEGIN
	SSMILLTOIJARR, 50., 225., rrow4xy, rcol4xy,pole,'E',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!550!9%'+polechar, /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = 2.

	SSMILLTOIJARR, 60., 225., rrow4xy, rcol4xy,pole,'E',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!560!9%'+polechar, /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = 2.

	SSMILLTOIJARR, 50., 45., rrow4xy, rcol4xy,pole,'W',85
	SSMILLTOIJARR, 50., 45+90., rrow4xy, rcol4xy,pole,'E',85
	;XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!545!9%!5W', /REMOVE_ALL), $
	;COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = 2.
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!5135!9%!5E', /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = 2.

	XYOUTS, FIX(nc/.9),nr-10, STRCOMPRESS('!10NASA', /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5
	;XYOUTS, 20,31, STRCOMPRESS('!5'+lbl$), $
	;COLOR=black_c+1, /DATA,ALIGNMENT = 0.,CHARSIZE = 1.,CHARTHICK = 2.

	SSMILLTOIJARR, 50., 45., rrow4xy, rcol4xy,pole,'E',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!545!9%!5E', /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = 2.
ENDELSE

END


pro latlonsub_thick,ltmn,ltmx,ltinc,lnmn,lnmx,lninc,pole,nc,nr,mag,black_c,lbl$, latlonsub_thick

PLOT,[0,1],[0,1],XRANGE=[0,nc*mag],XSTYLE=5, $
  YRANGE=[nr*mag,0],YSTYLE=5,/NODATA,/noerase, $
  position=[0,0,1,1]

latmin=double(ltmn)
latmax=double(ltmx)
latinc=double(ltinc)
lonmin=double(lnmn)
lonmax=double(lnmx)
loninc=double(lninc)

arrsize=FIX(((lonmax-lonmin)/loninc)+1+1)
rcoll=dblarr(arrsize)
rroww=dblarr(arrsize)
rlat=dblarr(arrsize)
rlon=dblarr(arrsize)
i=0.0D
j=0.0D

FOR i=latmin,latmax,latinc DO BEGIN
  nn=0
  FOR j=lonmin,lonmax,loninc DO BEGIN
     rlat(nn)=DOUBLE(i)
     rlon(nn)=DOUBLE(j)
     nn=nn+1
  ENDFOR
  ssmilltoijarr,rlat,rlon,rrowidl,rcolidl,pole,'W',85
  rcoll=rcolidl(0:arrsize-3)
  rroww=rrowidl(0:arrsize-3)
  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = linethickness
ENDFOR

arrsize2=FIX(((latmax-latmin)/latinc)+1+1)
rcoll=fltarr(arrsize2)
rroww=fltarr(arrsize2)
i=0.0D
j=0.0D
loninc=45
ip = 0
FOR j=lonmin,lonmax,loninc DO BEGIN
  nn=0
  FOR i=latmin,latmax,latinc DO BEGIN
    rlat(nn)=DOUBLE(i)
    rlon(nn)=DOUBLE(j)
    nn=nn+1
  ENDFOR
  ssmilltoijarr,rlat,rlon,rrowidl,rcolidl,pole,'W',85
  rcoll=rcolidl(0:arrsize2-3)
  rroww=rrowidl(0:arrsize2-3)
  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = linethickness
;
ip=ip+1
ENDFOR
    IF (pole EQ 1) THEN polechar = '!5N' ELSE polechar = '!5S'
IF (pole EQ 1) THEN BEGIN
	SSMILLTOIJARR, 50., 135., rrow4xy, rcol4xy,pole,'E',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!550!9%'+polechar, /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = linethickness

	SSMILLTOIJARR, 60., 135., rrow4xy, rcol4xy,pole,'E',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!560!9%'+polechar, /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = linethickness

	SSMILLTOIJARR, 60., 180., rrow4xy, rcol4xy,pole,'W',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!5180!9%!5E', /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = linethickness

	SSMILLTOIJARR, 60., 90., rrow4xy, rcol4xy,pole,'E',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!590!9%!5E', /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = linethickness
ENDIF ELSE BEGIN
	SSMILLTOIJARR, 50., 225., rrow4xy, rcol4xy,pole,'E',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!550!9%'+polechar, /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = linethickness

	SSMILLTOIJARR, 60., 225., rrow4xy, rcol4xy,pole,'E',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!560!9%'+polechar, /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = linethickness

	SSMILLTOIJARR, 50., 45., rrow4xy, rcol4xy,pole,'W',85
	SSMILLTOIJARR, 50., 45+90., rrow4xy, rcol4xy,pole,'E',85
	;XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!545!9%!5W', /REMOVE_ALL), $
	;COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = linethickness
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!5135!9%!5E', /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = linethickness

	XYOUTS, FIX(nc/.9),nr-10, STRCOMPRESS('!10NASA', /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5
	;XYOUTS, 20,31, STRCOMPRESS('!5'+lbl$), $
	;COLOR=black_c+1, /DATA,ALIGNMENT = 0.,CHARSIZE = 1.,CHARTHICK = linethickness

	SSMILLTOIJARR, 50., 45., rrow4xy, rcol4xy,pole,'E',85
	XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!545!9%!5E', /REMOVE_ALL), $
	COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = linethickness
ENDELSE

END

PRO latlonGAC_labelYN, ltmn,ltmx,ltinc,lnmn,lnmx,lninc,pole,nc,nr,mag,black_c,lbl$, LINETHICKNESS = linethickness, $
	LABELYN = labelYN, LABELTHICKNESS = labelthickness

IF KEYWORD_SET(linethickness) THEN thickofline = linethickness ELSE thickofline = 2
IF KEYWORD_SET(labelthickness) THEN thickoflbl = labelthickness ELSE thickoflbl = 2

	PLOT,[0,1],[0,1],XRANGE=[0,nc*mag],XSTYLE=5, $
	  YRANGE=[nr*mag,0],YSTYLE=5,/NODATA,/noerase, $
	  position=[0,0,1,1]

	latmin=double(ltmn)
	latmax=double(ltmx)
	latinc=double(ltinc)
	lonmin=double(lnmn)
	lonmax=double(lnmx)
	loninc=double(lninc)

	arrsize=FIX(((lonmax-lonmin)/loninc)+1+1)
	rcoll=dblarr(arrsize)
	rroww=dblarr(arrsize)
	rlat=dblarr(arrsize)
	rlon=dblarr(arrsize)
	i=0.0D
	j=0.0D

	FOR i=latmin,latmax,latinc DO BEGIN
	  nn=0
	  FOR j=lonmin,lonmax,loninc DO BEGIN
     	rlat(nn)=DOUBLE(i)
     	rlon(nn)=DOUBLE(j)
     	nn=nn+1
	  ENDFOR
	  ssmilltoijarr,rlat,rlon,rrowidl,rcolidl,pole,'W',85
	  rcoll=rcolidl(0:arrsize-3)
	  rroww=rrowidl(0:arrsize-3)
	  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = thickofline
	ENDFOR

	arrsize2=FIX(((latmax-latmin)/latinc)+1+1)
	rcoll=fltarr(arrsize2)
	rroww=fltarr(arrsize2)
	i=0.0D
	j=0.0D
	loninc=45
	ip = 0
	FOR j=lonmin,lonmax,loninc DO BEGIN
	  nn=0
	  FOR i=latmin,latmax,latinc DO BEGIN
	    rlat(nn)=DOUBLE(i)
	    rlon(nn)=DOUBLE(j)
	    nn=nn+1
	  ENDFOR
	  ssmilltoijarr,rlat,rlon,rrowidl,rcolidl,pole,'W',85
	  rcoll=rcolidl(0:arrsize2-3)
	  rroww=rrowidl(0:arrsize2-3)
	  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = thickofline
	  ;
	  ip=ip+1
	ENDFOR
	IF KEYWORD_SET(labelYN) THEN BEGIN 
    		IF (pole EQ 1) THEN polechar = '!5N' ELSE polechar = '!5S'
		IF (pole EQ 1) THEN BEGIN
			SSMILLTOIJARR, 50., 135., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!550!9%'+polechar, /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl

			SSMILLTOIJARR, 60., 135., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!560!9%'+polechar, /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl

			SSMILLTOIJARR, 60., 180., rrow4xy, rcol4xy,pole,'W',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!5180!9%!5E', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl

			SSMILLTOIJARR, 60., 90., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!590!9%!5E', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl
		ENDIF ELSE BEGIN
			SSMILLTOIJARR, 50., 225., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!550!9%'+polechar, /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl

			SSMILLTOIJARR, 60., 225., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!560!9%'+polechar, /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl

			SSMILLTOIJARR, 50., 45., rrow4xy, rcol4xy,pole,'W',85
			SSMILLTOIJARR, 50., 45+90., rrow4xy, rcol4xy,pole,'E',85
			;XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!545!9%!5W', /REMOVE_ALL), $
			;COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!5135!9%!5E', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl

			XYOUTS, FIX(nc/.9),nr-10, STRCOMPRESS('!10NASA', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5
			;XYOUTS, 20,31, STRCOMPRESS('!5'+lbl$), $
			;COLOR=black_c+1, /DATA,ALIGNMENT = 0.,CHARSIZE = 1.,CHARTHICK = thickoflbl

			SSMILLTOIJARR, 50., 45., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!545!9%!5E', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl
		ENDELSE
	ENDIF

END


PRO latlonGAC_labelYN_30S, ltmn,ltmx,ltinc,lnmn,lnmx,lninc,pole,nc,nr,mag,black_c,lbl$, LINETHICKNESS = linethickness, LABELYN = labelYN

IF KEYWORD_SET(linethickness) THEN thickofline = linethickness ELSE thickofline = 2

	PLOT,[0,1],[0,1],XRANGE=[0,nc*mag],XSTYLE=5, $
	  YRANGE=[nr*mag,0],YSTYLE=5,/NODATA,/noerase, $
	  position=[0,0,1,1]

	latmin=double(ltmn)
	latmax=double(ltmx)
	latinc=double(ltinc)
	lonmin=double(lnmn)
	lonmax=double(lnmx)
	loninc=double(lninc)

	arrsize=FIX(((lonmax-lonmin)/loninc)+1+1)
	rcoll=dblarr(arrsize)
	rroww=dblarr(arrsize)
	rlat=dblarr(arrsize)
	rlon=dblarr(arrsize)
	i=0.0D
	j=0.0D

	FOR i=latmin,latmax,latinc DO BEGIN
	  nn=0
	  FOR j=lonmin,lonmax,loninc DO BEGIN
     	rlat(nn)=DOUBLE(i)
     	rlon(nn)=DOUBLE(j)
     	nn=nn+1
	  ENDFOR
	  ssmilltoijarr_30S,rlat,rlon,rrowidl,rcolidl,pole,'W',85
	  rcoll=rcolidl(0:arrsize-3)
	  rroww=rrowidl(0:arrsize-3)
	  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = thickofline
	ENDFOR

	arrsize2=FIX(((latmax-latmin)/latinc)+1+1)
	rcoll=fltarr(arrsize2)
	rroww=fltarr(arrsize2)
	i=0.0D
	j=0.0D
	loninc=45
	ip = 0
	FOR j=lonmin,lonmax,loninc DO BEGIN
	  nn=0
	  FOR i=latmin,latmax,latinc DO BEGIN
	    rlat(nn)=DOUBLE(i)
	    rlon(nn)=DOUBLE(j)
	    nn=nn+1
	  ENDFOR
	  ssmilltoijarr_30S,rlat,rlon,rrowidl,rcolidl,pole,'W',85
	  rcoll=rcolidl(0:arrsize2-3)
	  rroww=rrowidl(0:arrsize2-3)
	  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = thickofline
	  ;
	  ip=ip+1
	ENDFOR
	IF KEYWORD_SET(labelYN) THEN BEGIN 
    		IF (pole EQ 1) THEN polechar = '!5N' ELSE polechar = '!5S'
		IF (pole EQ 1) THEN BEGIN
			SSMILLTOIJARR, 50., 135., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!550!9%'+polechar, /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickofline

			SSMILLTOIJARR, 60., 135., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!560!9%'+polechar, /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickofline

			SSMILLTOIJARR, 60., 180., rrow4xy, rcol4xy,pole,'W',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!5180!9%!5E', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickofline

			SSMILLTOIJARR, 60., 90., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!590!9%!5E', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickofline
		ENDIF ELSE BEGIN
			SSMILLTOIJARR, 50., 225., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!550!9%'+polechar, /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickofline

			SSMILLTOIJARR, 60., 225., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!560!9%'+polechar, /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickofline

			SSMILLTOIJARR, 50., 45., rrow4xy, rcol4xy,pole,'W',85
			SSMILLTOIJARR, 50., 45+90., rrow4xy, rcol4xy,pole,'E',85
			;XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!545!9%!5W', /REMOVE_ALL), $
			;COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickofline
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!5135!9%!5E', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickofline

			XYOUTS, FIX(nc/.9),nr-10, STRCOMPRESS('!10NASA', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5
			;XYOUTS, 20,31, STRCOMPRESS('!5'+lbl$), $
			;COLOR=black_c+1, /DATA,ALIGNMENT = 0.,CHARSIZE = 1.,CHARTHICK = thickofline

			SSMILLTOIJARR, 50., 45., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!545!9%!5E', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickofline
		ENDELSE
	ENDIF

END

PRO latlonGAC_labelYN2, ltmn,ltmx,ltinc,lnmn,lnmx,lninc,pole,nc,nr,mag,black_c,lbl$, LINETHICKNESS = linethickness, $
	LABELYN = labelYN, LABELTHICKNESS = labelthickness

IF KEYWORD_SET(linethickness) THEN thickofline = linethickness ELSE thickofline = 2
IF KEYWORD_SET(labelthickness) THEN thickoflbl = labelthickness ELSE thickoflbl = 2

	PLOT,[0,1],[0,1],XRANGE=[0,nc*mag],XSTYLE=5, $
	  YRANGE=[nr*mag,0],YSTYLE=5,/NODATA,/noerase, $
	  position=[0,0,1,1]

	latmin=double(ltmn)
	latmax=double(ltmx)
	latinc=double(ltinc)
	lonmin=double(lnmn)
	lonmax=double(lnmx)
	loninc=double(lninc)

	arrsize=FIX(((lonmax-lonmin)/loninc)+1+1)
	rcoll=dblarr(arrsize)
	rroww=dblarr(arrsize)
	rlat=dblarr(arrsize)
	rlon=dblarr(arrsize)
	i=0.0D
	j=0.0D

	FOR i=latmin,latmax,latinc DO BEGIN
	  nn=0
	  FOR j=lonmin,lonmax,loninc DO BEGIN
     	rlat(nn)=DOUBLE(i)
     	rlon(nn)=DOUBLE(j)
     	nn=nn+1
	  ENDFOR
	  ssmilltoijarr,rlat,rlon,rrowidl,rcolidl,pole,'W',85
	  rcoll=rcolidl(0:arrsize-3)
	  rroww=rrowidl(0:arrsize-3)
	  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = thickofline
	ENDFOR

	arrsize2=FIX(((latmax-latmin)/latinc)+1+1)
	rcoll=fltarr(arrsize2)
	rroww=fltarr(arrsize2)
	i=0.0D
	j=0.0D
	loninc=45
	ip = 0
	FOR j=lonmin,lonmax,loninc DO BEGIN
	  nn=0
	  FOR i=latmin,latmax,latinc DO BEGIN
	    rlat(nn)=DOUBLE(i)
	    rlon(nn)=DOUBLE(j)
	    nn=nn+1
	  ENDFOR
	  ssmilltoijarr,rlat,rlon,rrowidl,rcolidl,pole,'W',85
	  rcoll=rcolidl(0:arrsize2-3)
	  rroww=rrowidl(0:arrsize2-3)
	  PLOTS,rcoll*mag,rroww*mag,COLOR=black_c,NOCLIP=0,/DATA,THICK = thickofline
	  ;
	  ip=ip+1
	ENDFOR
	IF KEYWORD_SET(labelYN) THEN BEGIN 
    		IF (pole EQ 1) THEN polechar = '!5N' ELSE polechar = '!5S'
		IF (pole EQ 1) THEN BEGIN
			SSMILLTOIJARR, 50., 135., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!550!9%'+polechar, /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl

			SSMILLTOIJARR, 60., 135., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!560!9%'+polechar, /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl

			SSMILLTOIJARR, 60., 180., rrow4xy, rcol4xy,pole,'W',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!5180!9%!5E', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl

			SSMILLTOIJARR, 60., 90., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!590!9%!5E', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl
		ENDIF ELSE BEGIN
			SSMILLTOIJARR, 50., 225., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!550!9%'+polechar, /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl

			SSMILLTOIJARR, 60., 225., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!560!9%'+polechar, /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl

			SSMILLTOIJARR, 50., 45., rrow4xy, rcol4xy,pole,'W',85
			SSMILLTOIJARR, 50., 45+90., rrow4xy, rcol4xy,pole,'E',85
			;XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!545!9%!5W', /REMOVE_ALL), $
			;COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!5135!9%!5E', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl

			XYOUTS, FIX(nc/.9),nr-10, STRCOMPRESS('!10NASA', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5
			;XYOUTS, 20,31, STRCOMPRESS('!5'+lbl$), $
			;COLOR=black_c+1, /DATA,ALIGNMENT = 0.,CHARSIZE = 1.,CHARTHICK = thickoflbl

			SSMILLTOIJARR, 50., 45., rrow4xy, rcol4xy,pole,'E',85
			XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!545!9%!5E', /REMOVE_ALL), $
			COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = 1.7,CHARTHICK = thickoflbl
		ENDELSE
	ENDIF

END
