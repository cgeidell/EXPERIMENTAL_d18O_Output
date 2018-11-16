@/Volumes/KAB/bin/MkGACgifs/latlonGAC_wlC.pro
@/Volumes/KAB/bin/colorbar_avhrr1B.pro
;@/Volumes/KAB/bin/MkGACgifs/draw_cb4gif_CH1.pro
@/Volumes/KAB/bin/parseline
@/Volumes/KAB/bin/read_filenames
@/Volumes/KAB/bin/draw_cb4gif_chlora.pro
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

;maxcb = 5.D
;mincb = -15.D

maxcb = 0.D
mincb = -0.5

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
  
  ;POLYFILL, [663-1,690+2,690+2,663-1], $
  ;[332*2-38+2-4,332*2-38+2-4, 332*2-852,332*2-852], COLOR = BLACKc, /DEVICE, FILL_PATTERN = 0
  
  FOR m = 0, 200 DO BEGIN
      yst = cb_yst + m*hgt_of_cb/200.D
      yfn = cb_yst + (m+1.)*hgt_of_cb/200.D
      ;Tpr = Tmin + m*Tinc
      POLYFILL,[xst,xfn,xfn,xst],[yst,yst,yfn,yfn],COLOR = m+cb_skip,/NORMAL,FILL_PATTERN = 0
  ENDFOR

  lblV = [.01D,.02,.03,.05,.1,.2,.3,.5,1,2,3,5D,10,20,30,50D]
  lblV = MAKE_ARRAY(21, /LONG, /INDEX) + mincb
  FOR m = 0, N_ELEMENTS(lblV)-1 DO BEGIN
      ;clrout = m_cbar*ALOG10(lblV[m]) + b_cbar
      clrout = m_cbar*(lblV[m]) + b_cbar
      yst = cb_yst + clrout*hgt_of_cb/2000.D
      yfn = cb_yst + (clrout+1.D)*hgt_of_cb/2000.D
      ;XYOUTS, xst, yst + (yfn-yst)/2., '---', /NORMAL, ALIGNMENT = 0., SIZE = 1.3, COLOR = BLACKc+cb_skip
      ;XYOUTS, xst, yst , '___', /NORMAL, ALIGNMENT = 0., SIZE = 1.3, COLOR = BLACKc+cb_skip
      ;PRINT, [xst,xfn,xfn,xst]*(304.D*2.D + 90.D) 
      ;PRINT, [yst,yst,yst+.002D,yst+.002D]*448.D*2.D
      ;PRINT, [yst*448.D*2.D,yst*448.D*2.D,yst*448.D*2.D + 3.D/(448.D*2.D), yst*448.D*2.D + 3.D/(448.D*2.D)]
      ;PRINT, [yst*448.D*2.D,yst*448.D*2.D,yst*448.D*2.D + 1.D, yst*448.D*2.D + 1.D]
      ;PRINT, ' '
      
      ;NH
      ;nrow=448
      ;ncol=304
      ;SH
      ;nrow=332
      ;ncol=316
      
      ;POLYFILL,[xst, xfn - (xfn-xst)/2.D, xfn - (xfn-xst)/2.D, xst]*(304.D*2.D + 90.D), $
      ;         [yst*448.D*2.D,yst*448.D*2.D, yst*448.D*2.D + 1.D, yst*448.D*2.D + 1.D], $
      ;      COLOR = BLACKc+cb_skip, /DEVICE;, PATTERN = 0
      POLYFILL,[xst, xfn - (xfn-xst)/2.D, xfn - (xfn-xst)/2.D, xst]*(316.D*2.D + 90.D), $
        [yst*332.D*2.D,yst*332.D*2.D, yst*332.D*2.D + 1.D, yst*332.D*2.D + 1.D], $
        COLOR = BLACKc+cb_skip, /DEVICE;, PATTERN = 0      
            
            
      ;XYOUTS, xst-.006, yst + (yfn-yst)/2., STRCOMPRESS(STRING(FORMAT='(f5.0)', TPR), /REMOVE_ALL), /NORMAL, ALIGNMENT = 1., SIZE = 1.3
      TPR = lblV[m]
      IF (lblV[m] LT .1) THEN BEGIN
     XYOUTS, xst-.007D, yst-.005D, STRCOMPRESS(STRING(FORMAT='(f5.2)', TPR), /REMOVE_ALL), /NORMAL $
     , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
      ENDIF ELSE IF (lblV[m] LT 1.) THEN BEGIN
     XYOUTS, xst-.007D, yst-.005D, STRCOMPRESS(STRING(FORMAT='(f5.1)', TPR), /REMOVE_ALL), /NORMAL $
     , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
      ENDIF ELSE BEGIN
     XYOUTS, xst-.007D, yst-.005D, STRCOMPRESS(STRING(FORMAT='(f5.0)', TPR), /REMOVE_ALL), /NORMAL $
     , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
      ENDELSE
  ENDFOR
  ;XYOUTS, .2, .5, 'Chlorophyll Concentration, OC4 Algorithm' $
  ;, /NORMAL, ALIGNMENT = .5, SIZE = 1.2, COLOR = BLACKc+cb_skip, ORIENTATION = 90.
  ;XYOUTS, .4, .5, 'mg/m!U3!N', /NORMAL, ALIGNMENT = .5, SIZE = 1., COLOR = BLACKc+cb_skip $
  ;, ORIENTATION = 90.
END



PRO calcSSS_to_d18O

  ;pole = 'N'
  pole = 'S'
  pole$ = 'S'

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
    
    lm_map_25 = bytarr(ncol_ssmi,  nrow_ssmi)
    GET_LUN, filen
    OPENR, filen, '/Volumes/KAB/bin/south_landp_char_25'
    READU, filen, LM_HDR
    READU, filen, lm_map_25
    FREE_LUN, filen
    lm_map_12_5 = REBIN(lm_map_25, ncol_ssmi*2,  nrow_ssmi*2, /SAMPLE)
    ;lm_map_12_5 = REBIN(lm_map_25, ncol_ssmi*2, nrow_ssmi*2)
    
    ;map_lat = DBLARR(ncol_img, nrow_img)
    ;GET_LUN, filen
    ;OPENR, filen, '/Volumes/KAB/bin/lat'+ pole$ +'H_12_5km.dat.gz', /COMPRESS, /SWAP_IF_LITTLE_ENDIAN
    ;READU, filen, map_lat
    ;FREE_LUN, filen
    ;map_lat = ROTATE(ABS(map_lat), 7)

    map_lon = DBLARR(ncol_img, nrow_img)
    GET_LUN, filen
    OPENR, filen, '/Volumes/KAB/bin/lon'+ pole$ +'H_12_5km.dat.gz', /COMPRESS, /SWAP_IF_LITTLE_ENDIAN
    READU, filen, map_lon
    FREE_LUN, filen
    map_lon = ROTATE((map_lon), 7)
    
    map_lat = DBLARR(ncol_img, nrow_img)
    GET_LUN, filen
    OPENR, filen, '/Volumes/KAB/bin/latSH_12_5km_30S.dat.gz', /COMPRESS
    READU, filen, map_lat
    FREE_LUN, filen
    ;map_lat = REBIN(ROTATE(map_lat, 7), ncol_img, nrow_img)
    map_lat = ROTATE(ABS(map_lat), 7)  
  ENDIF
  
  indexcoast = WHERE(lm_map_12_5 EQ 2 OR lm_map_12_5 EQ 4, count)

  ; IC SECTORS
  ;locnames = ['a) N. Hemisphere', 'b) Arctic Ocean', 'c) Greenland Sea', 'd) Kara/Barents Seas', 'e) Bering Sea', 'f) Okhotsk/Japan Seas', $
  ;  'g) Canadian Archipelago', 'h) Baffin Bay/Labrador Sea', 'i) Hudson Bay', 'j) Gulf of St Lawrence']
  ;datarowpick = [11, 8, 6, 7, 3, 2, 9, 5, 4, 10]-2
  
;=====================================================================
;THIS IS THE SH SECTOR MASK:
;sectormsk = bytarr(ncol_img,nrow_img)
;GET_LUN, filen
;OPENR, filen, '/Volumes/Data6/Cynthia_library/sec125mask_char.dat';, /COMPRESS, /SWAP_IF_LITTLE_ENDIAN
;READU, filen, sectormsk
;FREE_LUN, filen
;WINDOW, 0, COLORS = 256, RETAIN = 2
;TVSCL, sectormsk*20
;=====================================================================

  ;datINT = INTARR(ncol_img, nrow_img)
  datINT = DBLARR(ncol_img, nrow_img)
  datSSS = DBLARR(ncol_img, nrow_img)
  imgflt = DBLARR(ncol_img, nrow_img)

  flsnames = FILE_SEARCH('/Users/CGE/Desktop/UIC/SPRING2018/Stable_Isotopes/Project/d18O_Gridding/SH_AquariusClimatology/sh_*_SESN_AVE_Aquarius_12_5DBL.gz', COUNT = nfls)
  ;/Users/CGE/Desktop/d18O_Project/SH_AquariusClimatology/sh_*_SESN_AVE_Aquarius_12_5DBL.gz

  ;fn = '/Users/CGE/Desktop/d18O_Project/SH_AquariusClimatology/sh_B_SPR_SESN_AVE_Aquarius_12_5DBL.gz'

  FOR ifl = 0L, nfls-1 DO BEGIN
    fn = flsnames[ifl]
    ;fn = flsnames[0]
    OPENR, 5, fn, /COMPRESS
    READU, 5, datINT
    CLOSE, 5
    datSSS[*,*] = -9999.D
    wgSSS = WHERE(datINT GT 0, cntgSSS)
    ;datSSS[wgSSS] = DOUBLE(datINT[wgSSS])/1000.D
    datSSS[wgSSS] = DOUBLE(datINT[wgSSS])
    PRINT, MAX(datINT), MIN(datINT)
    PRINT, MAX(datSSS), MIN(datSSS)
    ;WINDOW, 1, xsize = ncol_img, ysize = nrow_img, COLORS=256
    ;TVSCL, datSSS

    ;Southern Hemisphere Mask
    ;2 = Weddell Sea
    ;3 = Indian Ocean
    ;4 = W. Pacific Ocean
    ;5 = Ross Sea
    ;6 = Bellingshausen/Amundsen Seas

;STOP 

wSIAreaB = WHERE((datINT GT 0) AND map_lon GT (360.D-70) OR map_lon LE 10.D, nSIAreaB)
wSIAreaC = WHERE((datINT GT 0) AND map_lon GT  10.D AND map_lon LE  80.D, nSIAreaC)
wSIAreaD = WHERE((datINT GT 0) AND map_lon GT  80.D AND map_lon LE 150.D, nSIAreaD)
wSIAreaE = WHERE((datINT GT 0) AND map_lon GT 150.D AND map_lon LE 220.D, nSIAreaE)
wSIAreaF = WHERE((datINT GT 0) AND map_lon GT 220.D AND map_lon LE 290.D, nSIAreaF)

    ;datd18O = datSSS
    ;datd18O[wSIAreaB] = datSSS[wSIAreaB]*0.24 -8.45  ;Weddell Sea 
    ;datd18O[wSIAreaC] = datSSS[wSIAreaC]*0.16 -5.31  ;Indian Ocean 
    ;datd18O[wSIAreaD] = datSSS[wSIAreaD]*0.45 -15.29  ;W. Pacific Ocean 
    ;datd18O[wSIAreaE] = datSSS[wSIAreaE]*0.24 -8.45   ;Ross Sea 
    ;datd18O[wSIAreaF] = datSSS[wSIAreaF]*0.24 -8.45   ;B/Amundsen Seas 

    datd18O = datSSS
    datd18O[wSIAreaB] = datSSS[wSIAreaB]*0.24 -8.45  ;Weddell Sea
    datd18O[wSIAreaC] = datSSS[wSIAreaC]*0.24 -8.45  ;Indian Ocean
    datd18O[wSIAreaD] = datSSS[wSIAreaD]*0.24 -8.45  ;W. Pacific Ocean
    datd18O[wSIAreaE] = datSSS[wSIAreaE]*0.24 -8.45   ;Ross Sea
    datd18O[wSIAreaF] = datSSS[wSIAreaF]*0.24 -8.45   ;B/Amundsen Seas


;STOP
    
    WINDOW, 2, xsize = ncol_img, ysize = nrow_img, COLORS=256
    TVSCL, datd18O
    PRINT, 'MAX(datd18O)', MAX(datd18O)
    PRINT, 'MIN(datd18O)', MIN(datd18O)

    ;outpath$ = '/Users/CGE/Desktop/d18O_Project/'+ pole +'H_d18O_AQUARIUS_MONTHLY/'
    outpath$ = '/Volumes/KAB/'+ pole +'H_d18O_AQUARIUS_SEASONALL3/'
    SPAWN, 'mkdir '+ outpath$
    ;root$


    !P.FONT = 0

    ; COLOR BAR 201 colors
    ; INTERCEPT
    b_cbar = -200D*alog10(.01D)/(alog10(64.D) - alog10(.01D))
    ; SLOPE
    m_cbar = 200.D/(alog10(64.D) - alog10(.01D))
    ;PRINT,m_cbar*alog10(0.01)+b_cbar, m_cbar*alog10(64.)+b_cbar, m_cbar, b_cbar

    ;maxcb = 1.5
    ;mincb = -6.D

    maxcb = 0.0
    mincb = -0.5
    
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

    pseb1 = STRSPLIT(fn, '/', COUNT=nseb1, /EXTRACT)
    PRINT, pseb1, ' ', nseb1
    pseb2 = STRSPLIT(pseb1[nseb1-1], '.', COUNT=nseb2, /EXTRACT)
    root$ = pseb2[0]

    file2wrt = outpath$ + pole +'H_'+ root$ + '.dat.gz'
    OPENW, lls, file2wrt, /COMPRESS, /GET_LUN
    WRITEU, lls, datd18O
    FREE_LUN, lls

    datd18O = MEDIAN(datd18O, 3)
    datd18O = MEDIAN(datd18O, 5)
    datd18O = MEDIAN(datd18O, 7)

    imgflt[*,*] = 202
    gdat = WHERE(datd18O GT -999.D, cntnotzero)
    IF (cntnotzero GT 0) THEN BEGIN
      imgflt[gdat] = m_cbar*(datd18O[gdat]) + b_cbar
    ENDIF
    ;imgHOLD[*,*] = imgflt[*,*]

    ;TVLCT, 255/2., 255/2., 255/2., 204 ;GREY
    ;TVLCT, 255, 255, 255, 203 ;WHITE
    ;TVLCT, 0, 0, 0, 202 ;BLACK

    index_0 = WHERE(datd18O LT -999.D, count_0)
    indexLT = WHERE(datd18O GT -999.D AND datd18O LT mincb, countLT)
    indexGT = WHERE(datd18O GT maxcb, countGT)
    ;IF (count_0 GT 0) THEN imgflt(index_0) = 203 ;NO DATA - WHITE 
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

    ;latlonGAC_CHLOR_labelYN2, 30, 90, 10, 0, 360, .1, 1, ncol*2+xlen2add, nrow*2, 1, 202, ' '
    ;latlonASIA_PHIL, -20D,  50D,  10D, 90D,150D,  10D,ncol_img+xlen2add/1, nrow_img, 1, 202
    POLYFILL, [ncol_img,ncol_img+xlen2add,ncol_img+xlen2add,ncol_img], $
      [0-1, 0-1, nrow_img-1, nrow_img-1], COLOR = 203, /DEVICE, FILL_PATTERN = 0
    DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1"
    colorbarCHLOR_gif, 0.
    ;XYOUTS, 1.094,.0162, STRCOMPRESS('!10NASA!5', /REMOVE_ALL), $
    ;    COLOR=202, /NORMAL,ALIGNMENT = .5
    ;XYOUTS, 1.094,1.-m*.0162, STRMID(segs[n_segs-1],1,4), $
    ;    COLOR=202, /NORMAL,ALIGNMENT = .5
    ;PRINT, m,' ',STRMID(segs[n_segs-1],1,4)

    DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1"
    ;XYOUTS, .94-.005     , .96, 'mg/m', COLOR=34, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4
    ;XYOUTS, .94+.032-.005, .96+.005, '3', COLOR=34, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4
    ;XYOUTS, .94, .96, 'mg/m!U3!N', COLOR=34, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4
    ;XYOUTS, .935, .98, 'Chlorophyll', COLOR=34, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4
    ;XYOUTS, .94, .975, 'Concentration', COLOR=34, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4
    XYOUTS, .935, .98, 'd18O', COLOR=202, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4
    XYOUTS, .94-.005     , .96, 'o/oo', COLOR=202, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4

    date$ = SYSTIME(0)
    PARSELINE, date$, ' ', dateseg$, ndseg
    date$ = dateseg$[1]+' '+dateseg$[2]+', '+dateseg$[4]
    DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--11-80-100-100-p-60-iso10646-1"
    ; XYOUTS, .94, .005, date$, COLOR=202, /NORMAL, ALIGNMENT=.5, CHARSIZE=.9
    DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--18-180-75-75-p-103-iso8859-1"
    ;XYOUTS, .938, .03, imgdate$w, COLOR=199, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.3
    ; XYOUTS, .6, .94, imgdate$w, COLOR=205, /NORMAL, ALIGNMENT=.5, CHARSIZE=1., CHARTHICK = 2.
    TVLCT,r2,g2,b2,/GET
    bmap=TVRD()
    ;WRITE_GIF,outpath$+root$+'wDBOlocwklyAve.gif',bmap,r2,g2,b2
    ;PRINT,' ','Created: '+ outpath$ + root$+'wDBOlocwklyAveM1.gif'

    fl2wrt = outpath$ + root$ +'.gif'
    WRITE_GIF,fl2wrt,bmap,r2,g2,b2
    PRINT,'Created: '+fl2wrt
  ENDFOR



  STOP
END

