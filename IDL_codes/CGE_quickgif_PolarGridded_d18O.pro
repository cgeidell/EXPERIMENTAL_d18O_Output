@/Volumes/KAB/bin/MkGACgifs/latlonGAC_wlC.pro
@/Volumes/KAB/bin/parseline
@/Volumes/KAB/bin/read_filenames
@/Volumes/KAB/bin/latlonHIMA_1A.pro
@/Volumes/KAB/bin/ssmilltoijarr.pro


PRO latlonGAC_CHLOR_labelYN2, ltmn,ltmx,ltinc,lnmn,lnmx,lninc,pole,nc,nr,mag,black_c,lbl$, LINETHICKNESS = linethickness, $
  LABELYN = labelYN, LABELTHICKNESS = labelthickness, LABELSIZENESS = labelsizeness

IF KEYWORD_SET(linethickness) THEN thickofline = linethickness ELSE thickofline = 2
IF KEYWORD_SET(labelthickness) THEN thickoflbl = labelthickness ELSE thickoflbl = 2
IF KEYWORD_SET(labelsizeness) THEN sizeoflbl = labelsizeness ELSE sizeoflbl = 2.3

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
    PLOTS, rcoll*mag,rroww*mag, COLOR=black_c, NOCLIP=0, /DATA, THICK = thickofline
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
    PLOTS,rcoll*mag,rroww*mag, COLOR=black_c, NOCLIP=0, /DATA, THICK = thickofline
    ;
    ip=ip+1
  ENDFOR
  IF KEYWORD_SET(labelYN) THEN BEGIN 
        IF (pole EQ 1) THEN polechar = '!5N' ELSE polechar = '!5S'
    IF (pole EQ 1) THEN BEGIN
      SSMILLTOIJARR, 50., 135., rrow4xy, rcol4xy,pole,'E',85
      XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!550!5'+polechar, /REMOVE_ALL), $
      COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = sizeoflbl,CHARTHICK = thickoflbl

      SSMILLTOIJARR, 60., 135., rrow4xy, rcol4xy,pole,'E',85
      XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!560!5'+polechar, /REMOVE_ALL), $
      COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = sizeoflbl,CHARTHICK = thickoflbl

      SSMILLTOIJARR, 60., 180., rrow4xy, rcol4xy,pole,'W',85
      XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!5180!5E', /REMOVE_ALL), $
      COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = sizeoflbl,CHARTHICK = thickoflbl

      SSMILLTOIJARR, 60., 90., rrow4xy, rcol4xy,pole,'E',85
      XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!590!5E', /REMOVE_ALL), $
      COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = sizeoflbl,CHARTHICK = thickoflbl
    ENDIF ELSE BEGIN
      SSMILLTOIJARR, 50., 225., rrow4xy, rcol4xy,pole,'E',85
      XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!550!Eo!N'+polechar, /REMOVE_ALL), $
      COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = sizeoflbl,CHARTHICK = thickoflbl

      SSMILLTOIJARR, 60., 225., rrow4xy, rcol4xy,pole,'E',85
      XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!560!Eo!N'+polechar, /REMOVE_ALL), $
      COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = sizeoflbl,CHARTHICK = thickoflbl

      SSMILLTOIJARR, 50., 45., rrow4xy, rcol4xy,pole,'W',85
      SSMILLTOIJARR, 50., 45+90., rrow4xy, rcol4xy,pole,'E',85
      ;XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!545!Eo!N!5W', /REMOVE_ALL), $
      ;COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = sizeoflbl,CHARTHICK = thickoflbl
      XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!5135!Eo!N!5E', /REMOVE_ALL), $
      COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = sizeoflbl,CHARTHICK = thickoflbl

      XYOUTS, FIX(nc/.9),nr-10, STRCOMPRESS('!10NASA', /REMOVE_ALL), $
      COLOR=black_c, /DATA,ALIGNMENT = .5
      ;XYOUTS, 20,31, STRCOMPRESS('!5'+lbl$), $
      ;COLOR=black_c+1, /DATA,ALIGNMENT = 0.,CHARSIZE = 1.,CHARTHICK = thickoflbl

      SSMILLTOIJARR, 50., 45., rrow4xy, rcol4xy,pole,'E',85
      XYOUTS, rcol4xy,rrow4xy, STRCOMPRESS('!545!Eo!N!5E', /REMOVE_ALL), $
      COLOR=black_c, /DATA,ALIGNMENT = .5, CHARSIZE = sizeoflbl,CHARTHICK = thickoflbl
    ENDELSE
  ENDIF
END

PRO colorbarCHLOR_gif, cb_skip

  ; COLOR BAR 201 colors
  ; INTERCEPT
  b_cbar = -200D *alog10(.01D)/(alog10(64.D)- alog10(.01D))
  ; SLOPE
  m_cbar = 200.D/(alog10(64.D)- alog10(.01D))

  ; INTERCEPT
  b_cbar = -201D *alog10(.01D)/(alog10(64.D)- alog10(.01D))
  ; SLOPE
  m_cbar = 201.D/(alog10(64.D)- alog10(.01D))


;NH colorbar range
maxcb = 1.5
mincb = -6.D

;SH colorbar range
;maxcb = 0.D
;mincb = -0.5

; INTERCEPT
b_cbar = -200D*(mincb)/((maxcb) - (mincb))
; SLOPE
m_cbar = 200.D/((maxcb) - (mincb))

  WHITEc = 203
  TVLCT, 255, 255, 255, WHITEc ;WHITE
  BLACKc = 202
  TVLCT, 0, 0, 0, BLACKc ;BLACK
  ;ERASE, WHITEc+cb_skip

  xfac = 10.0D
  xfac = 8.D
  hgt_of_cb = .9D
  cb_yst = (1.D - hgt_of_cb)/2.D
  x_off = .95D
  Tpr = 0.0D
  xst = .7D
  xfn = (xst + 0.2D)
  xst =  x_off
  xfn = 1.D -.01D
  
  POLYFILL, [663-1,690+2,690+2,663-1], $
  [448*2-38+2-4,448*2-38+2-4, 448*2-852,448*2-852], COLOR = BLACKc, /DEVICE, FILL_PATTERN = 0
  
  FOR m = 0, 200 DO BEGIN
      yst = cb_yst + m*hgt_of_cb/200.D
      yfn = cb_yst + (m+1.)*hgt_of_cb/200.D
      ;Tpr = Tmin + m*Tinc
      POLYFILL,[xst,xfn,xfn,xst],[yst,yst,yfn,yfn],COLOR = m+cb_skip,/NORMAL,FILL_PATTERN = 0
  ENDFOR

  lblV = MAKE_ARRAY(21, /LONG, /INDEX) + mincb
  FOR m = 0, N_ELEMENTS(lblV)-1 DO BEGIN
      ;clrout = m_cbar*ALOG10(lblV[m]) + b_cbar
      clrout = m_cbar*(lblV[m]) + b_cbar
      yst = cb_yst + clrout*hgt_of_cb/200.D
      yfn = cb_yst + (clrout+1.D)*hgt_of_cb/200.D
      ;XYOUTS, xst, yst + (yfn-yst)/2., '---', /NORMAL, ALIGNMENT = 0., SIZE = 1.3, COLOR = BLACKc+cb_skip
      ;XYOUTS, xst, yst , '___', /NORMAL, ALIGNMENT = 0., SIZE = 1.3, COLOR = BLACKc+cb_skip
      ;PRINT, [xst,xfn,xfn,xst]*(304.D*2.D + 90.D) 
      ;PRINT, [yst,yst,yst+.002D,yst+.002D]*448.D*2.D
      ;PRINT, [yst*448.D*2.D,yst*448.D*2.D,yst*448.D*2.D + 3.D/(448.D*2.D), yst*448.D*2.D + 3.D/(448.D*2.D)]
      ;PRINT, [yst*448.D*2.D,yst*448.D*2.D,yst*448.D*2.D + 1.D, yst*448.D*2.D + 1.D]
      ;PRINT, ' '
      POLYFILL,[xst, xfn - (xfn-xst)/2.D, xfn - (xfn-xst)/2.D, xst]*(304.D*2.D + 90.D), $
               [yst*448.D*2.D,yst*448.D*2.D, yst*448.D*2.D + 1.D, yst*448.D*2.D + 1.D], $
            COLOR = BLACKc+cb_skip, /DEVICE;, PATTERN = 0
      ;XYOUTS, xst-.006, yst + (yfn-yst)/2., STRCOMPRESS(STRING(FORMAT='(f5.0)', TPR), /REMOVE_ALL), /NORMAL, ALIGNMENT = 1., SIZE = 1.3
      TPR = lblV[m]
   ;   IF (lblV[m] LT .1) THEN BEGIN
  ;  XYOUTS, xst-.007D, yst-.005D, STRCOMPRESS(STRING(FORMAT='(f5.2)', TPR), /REMOVE_ALL), /NORMAL $
  ;  , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
  ;    ENDIF ELSE IF (lblV[m] LT 1.) THEN BEGIN
  ;  XYOUTS, xst-.007D, yst-.005D, STRCOMPRESS(STRING(FORMAT='(f5.1)', TPR), /REMOVE_ALL), /NORMAL $
  ;  , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
  ;    ENDIF ELSE BEGIN
  ;  XYOUTS, xst-.007D, yst-.005D, STRCOMPRESS(STRING(FORMAT='(f5.0)', TPR), /REMOVE_ALL), /NORMAL $
  ;  , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
      ;ENDELSE
     XYOUTS, xst-.007D, yst-.005D, STRCOMPRESS(STRING(FORMAT='(f5.0)', TPR), /REMOVE_ALL), /NORMAL $
     , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
  ENDFOR
  ;XYOUTS, .2, .5, '<parameter being measured>' $
  ;, /NORMAL, ALIGNMENT = .5, SIZE = 1.2, COLOR = BLACKc+cb_skip, ORIENTATION = 90.
  ;XYOUTS, .4, .5, '<unit of parameter>', /NORMAL, ALIGNMENT = .5, SIZE = 1., COLOR = BLACKc+cb_skip $
  ;, ORIENTATION = 90.
END


PRO quickgif_Polar_d18O

pole = 'N'

LM_HDR = BYTARR(300)
IF((pole EQ 'N') OR (pole EQ 'n')) THEN BEGIN
  nrow=448L
  ncol=304L
  po=1
  nrow_ssmi =nrow
  ncol_ssmi =ncol
  nrow_img =nrow*2
  ncol_img =ncol*2
  nrow_avhr =nrow*4
  ncol_avhr =ncol*4
  lm_map_12_5 = BYTARR(ncol_img, nrow_img) 
  GET_LUN, filen
  OPENR, filen, '/Volumes/KAB/bin/north_landp_char_12_5'
  READU, filen, LM_HDR
  READU, filen, lm_map_12_5
  FREE_LUN, filen

  map_lon = DBLARR(ncol_img, nrow_img)
  GET_LUN, filen
  OPENR, filen, '/Volumes/KAB/bin/lonNH_12_5km.dat.gz', /COMPRESS, /SWAP_IF_LITTLE_ENDIAN
  READU, filen, map_lon
  FREE_LUN, filen
  map_lon = ROTATE(TEMPORARY(map_lon), 7)
  GET_LUN, filen

  map_lat = DBLARR(ncol_img, nrow_img)
  GET_LUN, filen
  OPENR, filen, '/Volumes/KAB/bin/latNH_12_5km.dat.gz', /COMPRESS, /SWAP_IF_LITTLE_ENDIAN
  READU, filen, map_lat
  FREE_LUN, filen
  map_lat = ROTATE(TEMPORARY(map_lat), 7)
  GET_LUN, filen
ENDIF


IF((pole EQ 'S') OR (pole EQ 's')) THEN BEGIN
  nrow=332
  ncol=316
  po=-1
  nrow_ssmi =nrow
  ncol_ssmi =ncol
  nrow_img =nrow*2
  ncol_img =ncol*2
  nrow_avhr =nrow*4
  ncol_avhr =ncol*4
  lm_map_12_5 = BYTARR(ncol_img, nrow_img) 
  GET_LUN, filen
  OPENR, filen, '/Volumes/KAB/bin/south_landp_char_12_5'
  READU, filen, LM_HDR
  READU, filen, lm_map_12_5
  FREE_LUN, filen
ENDIF
indexcoast = WHERE(lm_map_12_5 EQ 2 OR lm_map_12_5 EQ 4, count)

datd18O = DBLARR(ncol_img, nrow_img)
imgflt = DBLARR(ncol_img, nrow_img)

;flsnames = '/Users/CGE/Desktop/UIC/SPRING2018/Stable_Isotopes/Project/d18O_Gridding/FINAL_d18O_Output/NH_A_WIN_SESN_d18O_12_5kmDBL_AscDsc.dat'
flsnames = FILE_SEARCH('/Users/CGE/Desktop/UIC/SPRING2018/Stable_Isotopes/Project/d18O_Gridding/FINAL_d18O_Output/NH_*_SESN_d18O_12_5kmDBL_AscDsc.dat.gz', COUNT = nfls)

FOR ifl = 0L, nfls-1 DO BEGIN
  fn = flsnames[ifl]
  OPENR, 5, fn, /COMPRESS
  READU, 5, datd18O
  CLOSE, 5

  PRINT, MAX(datd18O), MIN(datd18O)

  ;PRINT, flsnames
  ;READU, 2, datd18O
  ;CLOSE, 2
  ;PRINT, ' '

  imgflt = DOUBLE(datd18O)

    WINDOW, 2, xsize = ncol_img, ysize = nrow_img, COLORS=256
    TVSCL, datd18O
    PRINT, 'MAX(datd18O)', MAX(datd18O)
    PRINT, 'MIN(datd18O)', MIN(datd18O)

    outpath$ = '/Volumes/KAB/'+ pole +'H_EXPd18O_GIF/'  ;Your output directory      
    SPAWN, 'mkdir '+ outpath$

    !P.FONT = 0

    ; COLOR BAR 201 colors
    ; INTERCEPT
    b_cbar = -200D*alog10(.01D)/(alog10(64.D) - alog10(.01D))
    ; SLOPE
    m_cbar = 200.D/(alog10(64.D) - alog10(.01D))
    ;PRINT,m_cbar*alog10(0.01)+b_cbar, m_cbar*alog10(64.)+b_cbar, m_cbar, b_cbar

    ;NH colorbar range
    maxcb = 1.5
    mincb = -6.D
    
    ;SH colorbar range 
    ;maxcb = 0.D
    ;mincb = -0.5

    ; INTERCEPT
    b_cbar = -200D*(mincb)/((maxcb) - (mincb))
    ; SLOPE
    m_cbar = 200.D/((maxcb) - (mincb))

    ;WINDOW, 0, COLORS=256, retain=2, xsize = 20, ysize = 25
    ;LOADCT, 33, NCOLORS = 201
    LOADCT, 22, NCOLORS = 201
    TVLCT, 255/2., 255/2., 255/2., 204 ;GREY
    TVLCT, 255, 255, 255, 203 ;WHITE
    TVLCT, 0, 0, 0, 202 ;BLACK
    PRINT, '!D.TABLE_SIZE = ', !D.TABLE_SIZE

    PRINT,'Current outpath is: ',outpath$
        
    ;datd18O = MEDIAN(datd18O, 3)
    ;datd18O = MEDIAN(datd18O, 5)

    pseb1 = STRSPLIT(fn, '/', COUNT=nseb1, /EXTRACT)
    PRINT, pseb1, ' ', nseb1
    pseb2 = STRSPLIT(pseb1[nseb1-1], '.', COUNT=nseb2, /EXTRACT)
    root$ = pseb2[0]
    
    imgflt[*,*] = 202
    gdat = WHERE(datd18O GT -999.D, cntnotzero)
    IF (cntnotzero GT 0) THEN BEGIN
      imgflt[gdat] = m_cbar*(datd18O[gdat]) + b_cbar
    ENDIF

    index_0 = WHERE(datd18O LT -999.D, count_0)
    indexLT = WHERE(datd18O GT -999.D AND datd18O LT mincb, countLT)
    indexGT = WHERE(datd18O GT maxcb, countGT)
    IF (count_0 GT 0) THEN imgflt(index_0) = 202 ;NO DATA - BLACK 
    IF (countGT GT 0) THEN imgflt(indexGT) = 200
    IF (countLT GT 0) THEN imgflt(indexLT) = 0

    imgflt[WHERE(lm_map_12_5 GT 0)] = 204 ; GREY for LAND
    imgflt[WHERE(lm_map_12_5 EQ 2 OR lm_map_12_5 EQ 4)] = 202 ; 

    xlen2add = 90
    WINDOW, 1, XSIZE=ncol*2+xlen2add, YSIZE=nrow*2, COLORS=255, /PIXMAP
    ERASE,203

    TV,imgflt
    DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1"
    latlonGAC_CHLOR_labelYN2, 30, 90, 10, 0, 360,.05, po, ncol*2+xlen2add, nrow*2, 1, 202, ' ', LINETHICKNESS = 1, $
      LABELYN = 1, LABELTHICKNESS = 2, LABELSIZENESS = 2.4D
    POLYFILL, [ncol_img,ncol_img+xlen2add,ncol_img+xlen2add,ncol_img], $
      [0-1, 0-1, nrow_img-1, nrow_img-1], COLOR = 203, /DEVICE, FILL_PATTERN = 0
    DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1"
    colorbarCHLOR_gif, 0.

    DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1"
    XYOUTS, .935, .98, 'd18O', COLOR=202, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4
    XYOUTS, .94-.005     , .96, 'o/oo', COLOR=202, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4

    date$ = SYSTIME(0)
    PARSELINE, date$, ' ', dateseg$, ndseg
    date$ = dateseg$[1]+' '+dateseg$[2]+', '+dateseg$[4]
    DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--11-80-100-100-p-60-iso10646-1"
    DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--18-180-75-75-p-103-iso8859-1"
 
    TVLCT,r2,g2,b2,/GET
    bmap=TVRD()
    fl2wrt = outpath$ + root$ +'.gif'
    WRITE_GIF,fl2wrt,bmap,r2,g2,b2
    PRINT,'Created: '+fl2wrt
  ENDFOR

;STOP
END
