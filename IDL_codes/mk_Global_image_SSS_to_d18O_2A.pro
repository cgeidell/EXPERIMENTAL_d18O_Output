@/Volumes/KAB/bin/parseline.pro
@/Volumes/KAB/bin/read_filenames.pro
@/Volumes/KAB/bin/colorbar_avhrr1T_43.pro
@/Volumes/KAB/bin/steve_latlontobox_xy.pro

@/Volumes/KAB/bin/latlonGAC_wlC.pro
@/Volumes/KAB/bin/colorbar_avhrr1B.pro
@/Volumes/KAB/bin/parseline
@/Volumes/KAB/bin/read_filenames
@/Volumes/KAB/bin/draw_cb4gif_chloraSE2.pro
;@/Volumes/KAB/bin/draw_cb4gif_chlora.pro
@/Volumes/KAB/bin/latlonHIMA_1A.pro
@/Volumes/KAB/bin/ssmilltoijarr.pro

@/Volumes/KAB/bin/latlonGAC_wlC.pro
@/Volumes/KAB/bin/colorbar_avhrr1B.pro
@/Volumes/KAB/bin/parseline.pro
@/Volumes/KAB/bin/read_filenames.pro
@/Volumes/KAB/bin/draw_cb4gif_chloraSE2.pro
;@/Volumes/KAB/bin/draw_cb4gif_chlora.pro
@/Volumes/KAB/bin/latlonHIMA_1A.pro
@/Volumes/KAB/bin/ssmilltoijarr.pro


PRO colorbarSSS_gif, pole, cb_skip, szdat

	; COLOR BAR 201 colors
	; INTERCEPT
	b_cbar = -200D *alog10(.01D)/(alog10(64.D)- alog10(.01D))
	; SLOPE
	m_cbar = 200.D/(alog10(64.D)- alog10(.01D))

	; INTERCEPT
	b_cbar = -201D * (28.D)/((38.D)- (28.D))
	; SLOPE
	m_cbar = 201.D/((38.D)- (28.D))

	; INTERCEPT
	b_cbar = -201D * (25.0D)/((39.D)- (25.0D))
	; SLOPE
	m_cbar = 201.D/((39.D)- (25.0D))

	WHITEc = 203
	TVLCT, 255, 255, 255, WHITEc ;WHITE
	BLACKc = 202
	TVLCT, 0, 0, 0, BLACKc ;BLACK
	;ERASE, WHITEc+cb_skip

	xfac = 10.0D
	xfac = 8.D
	;hgt_of_cb = .9D
	hgt_of_cb = .8D
	cb_yst = (1.D - hgt_of_cb)/2.D
	;x_off = .95D
	Tpr = 0.0D
	;xst = .7D
	;xfn = (xst + 0.2D)
	x_off = .98D
	xst =  x_off
	xfn = 1.D -.005D
	
	;POLYFILL, [663-1,690+2,690+2,663-1], $
	;[448*2-38+2-4,448*2-38+2-4, 448*2-852,448*2-852], COLOR = BLACKc, /DEVICE, FILL_PATTERN = 0
	
	IF pole EQ 1 THEN BEGIN
		POLYFILL, [663-1,690+2,690+2,663-1], $
		[448*2-38+2-4,448*2-38+2-4, 448*2-852,448*2-852], COLOR = BLACKc, /DEVICE, FILL_PATTERN = 0
	ENDIF ELSE IF pole EQ -1 THEN BEGIN
		POLYFILL, [686-2,714+2,714+2,686-1], $
		[332*2-29+0,332*2-29+0, 332*2-629-3,332*2-629-3], COLOR = BLACKc, /DEVICE, FILL_PATTERN = 0
	ENDIF
	
	FOR m = 0, 200 DO BEGIN
	    yst = cb_yst + m*hgt_of_cb/200.D
	    yfn = cb_yst + (m+1.)*hgt_of_cb/200.D
	    ;Tpr = Tmin + m*Tinc
	    POLYFILL,[xst,xfn,xfn,xst],[yst,yst,yfn,yfn],COLOR = m+cb_skip,/NORMAL,FILL_PATTERN = 0
	ENDFOR

	lblV = [.01D,.02,.03,.05,.1,.2,.3,.5,1,2,3,5D,10,20,30,50D]
	lblV = MAKE_ARRAY(11, /INDEX, /DOUB)+28.D
	lblV = MAKE_ARRAY(11, /INDEX, /DOUB)*.5D + 0.D
	lblV = MAKE_ARRAY(15, /INDEX, /DOUB)+25.D
	FOR m = 0, N_ELEMENTS(lblV)-1 DO BEGIN
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
;	IF pole EQ 1 THEN BEGIN
;	    POLYFILL,[xst, xfn - (xfn-xst)/2.D, xfn - (xfn-xst)/2.D, xst]*(304.D*2.D + 90.D), $
;	             [yst*448.D*2.D,yst*448.D*2.D, yst*448.D*2.D + 1.D, yst*448.D*2.D + 1.D], $
;	    			COLOR = BLACKc+cb_skip, /DEVICE;, PATTERN = 0
;	ENDIF ELSE BEGIN
;	    POLYFILL,[xst, xfn - (xfn-xst)/2.D, xfn - (xfn-xst)/2.D, xst]*(316.D*2.D + 90.D), $
;	             [yst*332.D*2.D,yst*332.D*2.D, yst*332.D*2.D + 1.D, yst*332.D*2.D + 1.D], $
;	    			COLOR = BLACKc+cb_skip, /DEVICE;, PATTERN = 0
;	ENDELSE
	    POLYFILL,[xst, xfn - (xfn-xst)/2.D, xfn - (xfn-xst)/2.D, xst]*(szdat[0] + 90.D), $
	             [yst*szdat[1],yst*szdat[1], yst*szdat[1] + 1.D, yst*szdat[1] + 1.D], $
	    			COLOR = BLACKc+cb_skip, /DEVICE;, PATTERN = 0
				
	    ;XYOUTS, xst-.006, yst + (yfn-yst)/2., STRCOMPRESS(STRING(FORMAT='(f5.0)', TPR), /REMOVE_ALL), /NORMAL, ALIGNMENT = 1., SIZE = 1.3
	    TPR = lblV[m]
	    ;IF (lblV[m] LT .1) THEN BEGIN
		; XYOUTS, xst-.007D, yst-.005D, STRCOMPRESS(STRING(FORMAT='(f5.2)', TPR), /REMOVE_ALL), /NORMAL $
		; , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
	    ;ENDIF ELSE IF (lblV[m] LT 1.) THEN BEGIN
		; XYOUTS, xst-.007D, yst-.005D, STRCOMPRESS(STRING(FORMAT='(f5.1)', TPR), /REMOVE_ALL), /NORMAL $
		; , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
	    ;ENDIF ELSE BEGIN
	    	 IF m EQ 0 THEN BEGIN
		 	strLBL = '<'+ STRCOMPRESS(STRING(FORMAT='(f5.1)', TPR), /REMOVE_ALL)
		 ENDIF ELSE IF m EQ (N_ELEMENTS(lblV)-1) THEN BEGIN
		 	strLBL = '>'+ STRCOMPRESS(STRING(FORMAT='(f5.1)', TPR), /REMOVE_ALL)
		 ENDIF ELSE BEGIN
		 	strLBL = STRCOMPRESS(STRING(FORMAT='(f5.1)', TPR), /REMOVE_ALL)
		 ENDELSE
		 XYOUTS, xst-.007D, yst-.005D, strLBL, /NORMAL $
		 , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
	   ; ENDELSE
	    
	ENDFOR
	;XYOUTS, .2, .5, 'Chlorophyll Concentration, OC4 Algorithm' $
	;, /NORMAL, ALIGNMENT = .5, SIZE = 1.2, COLOR = BLACKc+cb_skip, ORIENTATION = 90.
	;XYOUTS, .4, .5, 'mg/m!U3!N', /NORMAL, ALIGNMENT = .5, SIZE = 1., COLOR = BLACKc+cb_skip $
	;, ORIENTATION = 90.


END


PRO Grid_Global
	llinc = 1.D/24.D
	llinc = .25; deg
	nrowYimg = 180L * 4L
	ncolXimg = 360L*4L;*2L
	
	glat = DBLARR(ncolXimg, nrowYimg)
	glon = DBLARR(ncolXimg, nrowYimg)
	latarr = MAKE_ARRAY(nrowYimg, /DOUBLE, /INDEX)*llinc + llinc/2.D - 90.D
	FOR ip = 0L, ncolXimg-1 DO glat[ip, *] = latarr[*]
	lonarr = MAKE_ARRAY(ncolXimg, /DOUBLE, /INDEX)*llinc + llinc/2.D
	FOR ip = 0L, nrowYimg-1 DO glon[*, ip] = lonarr[*]

;STOP

	;fgdnms$ = FILE_SEARCH('/diskd/DATA/MODIS_Aqua_Mapped_Monthly_4km_SST/A*.L3m_MO_SST_sst_4km.nc', COUNT = nfls)
	fgdnms$ = FILE_SEARCH('/diskd/DATA/AQUARIUS_L3/podaac-ftp.jpl.nasa.gov/allData/aquarius/L3/mapped/V4/7day_running/SCI/2015/158/Q*.L3m_R7_SCI_V4.0_SSS_1deg', COUNT = nfls)
; 360x360
	
	llinc = 1.D/24.D
	llinc = .25; deg
	nrowYimg = 180L*4L
	ncolXimg = 360L*4L;*2L
	imgflt = DBLARR(ncolXimg, nrowYimg)
	imgHOLD = DBLARR(ncolXimg, nrowYimg)
	
	sectormsk = DBLARR(ncolXimg, nrowYimg)
	dataSSS = DBLARR(ncolXimg, nrowYimg)
	
	OPENR, 6, 'globalNHsectorMapDBL_1B.dat'
	READU, 6, sectormsk
	CLOSE, 6
    PRINT, 'MAX(sectormsk) = ', MAX(sectormsk)
    PRINT, 'MIN(sectormsk) = ', MIN(sectormsk)

	
	;fgdnms$ = FILE_SEARCH('/disk1/larrys/SSS/gAquarAveAllL3_7dayAve_DBLpt25LL.dat', COUNT = nfls)
	fgdnms$ = FILE_SEARCH( '/Volumes/KAB/GISS_d18OdD/gAquarAveAllL3_7dayAve_DBLpt25LL.dat', COUNT = nfls)
	ifl = 0	
    inpfnSST = fgdnms$[ifl]
    OPENR, ffo, inpfnSST, /COMPRESS, /GET_LUN
    READU, ffo, dataSSS
    FREE_LUN, ffo
    PRINT, 'MAX(dataSSS) = ', MAX(dataSSS)
    PRINT, 'MIN(dataSSS) = ', MIN(dataSSS)

	
	wareaA = WHERE((dataSSS GT 0) AND (sectormsk EQ 8 OR sectormsk EQ 9 OR sectormsk EQ 7), cntA)
	wareaB = WHERE((dataSSS GT 0) AND (sectormsk EQ 6 OR sectormsk EQ 13), cntB)
	wareaC = WHERE((dataSSS GT 0) AND (sectormsk EQ 5) AND (glat GE 62.D), cntC)
	wareaD = WHERE((dataSSS GT 0) AND (sectormsk EQ 5) AND (glat LT 62.D), cntD)
	wareaE = WHERE((dataSSS GT 0) AND (sectormsk EQ 4), cntE)
	wareaF = WHERE((dataSSS GT 0) AND (sectormsk EQ 1) AND ((glon LT 90.D) OR (glon GT 270.D)), cntD)
	wareaG = WHERE((dataSSS GT 0) AND (sectormsk EQ 10), cntB)
	wareaH = WHERE((dataSSS GT 0) AND (sectormsk EQ 1 OR sectormsk EQ 14) AND (glon GT 90.D) AND (glon LT 270.D), cntD)
	wareaI = WHERE((dataSSS GT 0) AND (sectormsk EQ 2 OR sectormsk EQ 3), cntI)
	
;A #8#9#7#9	Arctic Ocean 1846 0.48 -16.82 0.007 0.234 0.690 \u2013 0
;B #6#13	blue GIN Seas 684 0.60 -20.71 0.014 0.466 0.744 0.224 6
;C #5 > 62 N Baffin Bay 492 0.33 -11.82 0.14 0.461 0.528 \u2013 0
;D #5 < 62 Labrador Sea 647 0.94 -32.45 0.014 0.490 0.87 \u2013 0
;E #4 Hudson Bay 376 0.42 -16.05 0.024 0.768 0.462 \u2013 0
;F #10(#1 lon < 90 lon > 270) North Atlantic 743 0.55 -18.98 0.005 0.156 0.951 0.193 23
; South Atlantic 61 0.51 -17.40 0.013 0.449 0.963 0.118 15
;G #10 North Atlantic 743 0.55 -18.98 0.005 0.156 0.951 0.193 23
;H #1 lon > 90 lon < 270 ok North Pacific 751 0.44 -15.13 0.007 0.229 0.834 0.168 16
;I #2#3  ok North Pacific 751 0.44 -15.13 0.007 0.229 0.834 0.168 16

		datd18O = DBLARR(ncolXimg, nrowYimg)
		datUSED = INTARR(ncolXimg, nrowYimg)
		datd18O[*,*] = -99999999.D
		datd18O[wareaA] = dataSSS[wareaA]*0.48 -16.82 ;Arctic 
		datd18O[wareaB] = dataSSS[wareaB]*0.60 -20.71 ;GIN 
		datd18O[wareaC] = dataSSS[wareaC]*0.33 -11.82 ;Baffin Bay 
		datd18O[wareaD] = dataSSS[wareaD]*0.94 -32.45 ;Labrador
		datd18O[wareaE] = dataSSS[wareaE]*0.42 -16.05 ;Hudson Bay 
		datd18O[wareaF] = dataSSS[wareaF]*0.55 -18.98 ;North Atlantic 
		datd18O[wareaG] = dataSSS[wareaG]*0.55 -18.98 ;North Atlantic 
		datd18O[wareaH] = dataSSS[wareaH]*0.44 -15.13 ;North Pacific 
		datd18O[wareaI] = dataSSS[wareaI]*0.44 -15.13 ;North Pacific 
		
		datUSED[wareaA] = 1
		datUSED[wareaB] = 1
		datUSED[wareaC] = 1
		datUSED[wareaD] = 1
		datUSED[wareaE] = 1
		datUSED[wareaF] = 1 
		datUSED[wareaG] = 1
		datUSED[wareaH] = 1 
		datUSED[wareaI] = 1

;Tropical Atlantic 285 0.15 -4.61
;South Atlantic 61 0.51 -17.40
;;Mediterranean Sea 131 0.28 -9.24
;;Red Sea/Persian Gulf 36 0.31 -10.81
;North Pacific 751 0.44 -15.13 0.007 ; 
;Tropical Pacific 286 0.27 -8.88 ; 20 - 20
;South Pacific 19 0.45 -15.29 ; 20 - 50
;;Indian Ocean 332 0.16 -5.31 ; 
;Southern Ocean 503 0.24 -8.45 ; >50

	; Tropical Atlantic 285 0.15 -4.61
	;wareaA = WHERE(((dataSSS GT 0) AND (datd18O GT -99998.D) AND (glat LE 20.D) and (glat GE -20.D))  AND ((glon LT 90.D) OR (glon GT 270.D)), cntA)
	wareaA = WHERE(((dataSSS GT 0) AND (datUSED NE 1)  AND (glat LE 10.D) and (glat GE -10.D))  AND ((glon LT 90.D) OR (glon GT 270.D)), cntA)
		datd18O[wareaA] = dataSSS[wareaA]*0.55 -18.98 ;used slope and y-intercept of North Atlantic 
		;datd18O[wareaA] = dataSSS[wareaA]*0.15 -4.61

		datUSED[wareaA] = 1
	
	; South Atlantic 61 0.51 -17.40
	wareaB = WHERE((dataSSS GT 0) AND (datUSED NE 1) AND (glat LE -10.D) AND (glat GE -60.D) AND ((glon LT 90.D) OR (glon GT 270.D)), cntB)
		;datd18O[wareaB] = dataSSS[wareaB]*0.51 -17.40
		datd18O[wareaB] = dataSSS[wareaB]*0.55 -18.98 ; use slope and y-intercept of North Atlantic 
	datUSED[wareaB] = 1
	
	; NORTH Atlantic North Atlantic 743 0.55 -18.98
	wareaB = WHERE((dataSSS GT 0) AND (datUSED NE 1) AND (glat GT 10.D) AND ((glon LT 90.D) OR (glon GT 270.D)), cntB)
		datd18O[wareaB] = dataSSS[wareaB]*0.55 -18.98
	datUSED[wareaB] = 1
	
	; North Pacific 751 0.44 -15.13 0.007 ; 
	wareaC = WHERE((dataSSS GT 0) AND (datUSED NE 1) AND (glat GE 10.D) AND (glon GT 90.D) AND (glon LT 270.D), cntC)
		datd18O[wareaC] = dataSSS[wareaC]*0.44 -15.13
	datUSED[wareaC] = 1
	
	; Tropical Pacific 286 0.27 -8.88 ; 20 - 20
	wareaD = WHERE((dataSSS GT 0) AND (datUSED NE 1) AND (glat LT 10.D) and (glat GE -10.D) AND (glon GT 90.D) AND (glon LT 270.D), cntD)
		datd18O[wareaD] = dataSSS[wareaD]*0.44 -15.13 ;used slope and y-intercept of North Pacific 
		;datd18O[wareaD] = dataSSS[wareaD]*0.27 -8.88 
	datUSED[wareaD] = 1
	
	; South Pacific 19 0.45 -15.29 ; 20 - 50
	wareaE = WHERE((dataSSS GT 0) AND (datUSED NE 1) AND (glat LE -10.D) AND (glat GE -60.D) AND (glon GT 90.D) AND (glon LT 270.D), cntE)
		datd18O[wareaE] = dataSSS[wareaE]*0.44 -15.13 ;used slope and y-intercept of North Pacific 
		;datd18O[wareaE] = dataSSS[wareaE]*0.45 -15.29
	
	; Southern Ocean 503 0.24 -8.45 ; >50
	wareaF = WHERE((dataSSS GT 0.D) AND (glat LE -60.D), cntF)
		datd18O[wareaF] = dataSSS[wareaF]*0.24 -8.45
			
	!P.FONT = 0
	
	;WINDOW, 0, COLORS=256, retain=2, xsize = 20, ysize = 25
	; SSS
	;LOADCT, 33, NCOLORS = 201
	; d18O
	LOADCT, 22, NCOLORS = 201
	
	TVLCT, 255/2., 255/2., 255/2., 204 ;GREY
	TVLCT, 255, 255, 255, 203 ;WHITE
	TVLCT, 0, 0, 0, 202 ;BLACK
	PRINT, '!D.TABLE_SIZE = ', !D.TABLE_SIZE
	BLACK = 202
	WHITE = 203
	GREY  = 204

	rd = [0., 1., 1., 0., 0., 1., .43 , 1., 1.00, 1.,      .5]
	gr = [0., 1., 0., 1., 0., 1., .875, 0., .302, 156./255.,      .5]
	bl = [0., 1., 0., 0., 1., 0., 1.00, 1., 0.00, 0.00,      .5]
	TVLCT, 255*rd, 255*gr, 255*bl, 205L
	;BLACK = 0
	;WHITE = 1
	;RED = 2
	;GREEN = 3
	;BLUE = 4
	;YELLOW = 5
	;TURkish blue = 6
	;CYAN 7
	;ORANGE 8
	;GOLD 9

	;SPAWN, 'mkdir '+ outpath$
	;outpath$ = '/disk1/larrys/SSS/'
  outpath$ = '/Volumes/KAB/GISS_d18OdD/Global_d18O_test/'
	
	fgdnms$ = FILE_SEARCH('/Volumes/KAB/GISS_d18OdD/gAquarAveAllL3_7dayAve_DBLpt25LL.dat', COUNT = nfls)

; global .25 deg
	
	;STOP
	
	;lnpsd1 = STRSPLIT(fn2open$, '/', /EXTRACT, COUNT=npsd1)
	;lnpsd2 = STRSPLIT(lnpsd1[npsd1-1], '.', /EXTRACT, COUNT=npsd2)
	;moinp = LONG(STRMID(lnpsd2[0], 4, 2))
	;yrinp =  LONG(STRMID(lnpsd2[0], 0, 4))
	;rootout$ = 'a'+ STRING(yrinp, FORMAT='(I4.4)') + STRING(moinp, FORMAT='(I2.2)') +'_MO_COMBLSTSST_MODIS'+ MODIS_AorT +'_360x360'
	
	rootout$ = 'Gl_SSS_pt25deg'
	rootout$ = 'Gl_d18O_pt25deg'
	fl2wrt = outpath$ + rootout$
	imgdate$w = ' '
	
	;PRINT, 'fl2wrt = ', fl2wrt
	;flcheck = FILE_TEST(fl2wrt)
	;STOP
	
	file2wrt = 'Global_d18O_pt25deg_DBL.gz'
	OPENW, lls, file2wrt, /COMPRESS, /GET_LUN
	WRITEU, lls, datd18O
	FREE_LUN, lls
	
	szf = SIZE(datd18O, /DIM)
  ;d18O_32x = REBIN((datd18O), szf[0]*3L, szf[1]*3L, /INTERP, /MINUS_ONE)
	;d18O_32x = datd18O
	
	datd18O = MEDIAN(datd18O,3)
  ;datd18O = MEDIAN(datd18O,5)
  ;datd18O = MEDIAN(datd18O,7)
  ;datd18O = MEDIAN(datd18O,9)
  ;datd18O = MEDIAN(datd18O,11)

	SST_FIN = datd18O
	PRINT, 'MAX(SST_FIN) = ', MAX(SST_FIN)
	PRINT, 'MIN(SST_FIN) = ', MIN(SST_FIN)
	
	imgcb_st = 25.D
	imgcb_fn = 39.D
	;imgcb_st = 28.D
	;imgcb_fn = 38.D
	imgcb_st = -6.D
	imgcb_fn =  2.D

	; INTERCEPT
	b_cbar = -201D * (imgcb_st)/((imgcb_fn)- (imgcb_st))
	; SLOPE
	m_cbar = 201.D/((imgcb_fn)- (imgcb_st))
	
	szphil = SIZE(dataSSS, /DIM)
	ncolXimg = szphil[0]
	nrowYimg = szphil[1]

	wgdat = WHERE(SST_FIN GT -999D, countOK)

	wdatGT0 = WHERE(SST_FIN GT -998D, countLT)
	imgflt[*,*] = -9999999.D
    imgflt[wdatGT0] = SST_FIN[wdatGT0]; - 273.15D
    index_0 = WHERE(SST_FIN LE -998D, count_0)
    indexGT = WHERE(imgflt GT imgcb_fn, countGT)
    indexLT = WHERE(imgflt LT imgcb_st AND imgflt GT -99998.D, countLT)
    ;imgflt[wdatGT0] = (imgflt[wdatGT0] - imgcb_st + .5D*inc_T)/inc_T + 0.D;
    imgflt[wgdat] = m_cbar*(SST_FIN[wgdat]) + b_cbar + 0.D;

    imgHOLD[*,*] = imgflt[*,*]
	;imgflt[*,*] = imgHOLD[*,*]

	;IF (count_0 GT 0) THEN imgflt(index_0) = WHITE
	IF (count_0 GT 0) THEN imgflt(index_0) = BLACK ;NO DATA
	IF (countGT GT 0) THEN imgflt(indexGT) = 200
	IF (countLT GT 0) THEN imgflt(indexLT) = 0
	imgflt[*, nrowYimg-1] = BLACK
	imgflt[ncolXimg-1, *] = BLACK

	;imgflt[WHERE(lm_map_12_5 GT 0)] = 204 ; WHITE
	;imgflt[WHERE(lm_map_12_5 EQ 2 OR lm_map_12_5 EQ 4)] = 202 ;
	
	latst = -90.D
	latfn =  90.D
	lonst = 0.D
	lonfn = 360.D
	 
	
	xlen2add = 60; 
	xlen2add = 0; 
	WINDOW, 1, XSIZE=szphil[0]+xlen2add, YSIZE=szphil[1], COLORS=255, /PIXMAP
	ERASE,WHITE

	TV,imgflt

	DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1"
	;latlonGAC_CHLOR_labelYN2, 30, 90, 10, 0, 360,.05, po, ncol*2+xlen2add, nrow*2, 1, 205, ' ', LINETHICKNESS = 1, $
	;	LABELYN = 1, LABELTHICKNESS = 2, LABELSIZENESS = 2.4D

	DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--18-180-75-75-p-103-iso8859-1"

	;MAP_SET, /CYLINDRICAL, 0, 122.5, LIMIT=[4.8,114.9,19.7,129.78], /NOBORDER, /CONTINENTS, /NOERASE, CON_COLOR = BLACK, /HIRES, XMARGIN = 0, YMARGIN = 0, $
	
	;MAP_SET, /CYLINDRICAL, 0, 122.5, LIMIT=[5D, 115,20.D, 130.D + xlen2add*llinc], /NOBORDER, /CONTINENTS, /NOERASE, CON_COLOR = BLACK, /HIRES, XMARGIN = 0, YMARGIN = 0, $
	MAP_SET, /CYLINDRICAL, 0, 180, LIMIT=[latst, lonst, latfn, lonfn + xlen2add*llinc], /NOBORDER, /CONTINENTS, /NOERASE, CON_COLOR = BLACK, /HIRES, XMARGIN = 0, YMARGIN = 0;, $
	;/GRID, GLINESTYLE =0, LONDEL = 30, LATDEL = 30, LATALIGN = 0.50D, LONALIGN = 0.0D, MLINETHICK = 1, GLINETHICK = 1, E_GRID={COLOR:BLACK};, E_GRID={LABEL:2, COLOR:BLACK}

	MAP_CONTINENTS, /HIRES, /COUNTRIES, /COASTS, COLOR = GREY,  MLINETHICK = 1, /FILL_CONTINENTS
	MAP_CONTINENTS, /HIRES, /COUNTRIES, /COASTS, COLOR = BLACK,  MLINETHICK = 1;, /FILL_CONTINENTS
	;MAP_GRID,  LONDEL = 30, LATDEL = 30, COLOR = BLACK, GLINESTYLE =0

	;latlonASIA_PHIL, -20D,  50D,  10D, 90D,150D,  10D,ncolXimg+xlen2add/1, nrowYimg, 1, 202
	POLYFILL, [ncolXimg,ncolXimg+xlen2add,ncolXimg+xlen2add,ncolXimg], $
			[0-1, 0-1, nrowYimg, nrowYimg], COLOR = WHITE, /DEVICE, FILL_PATTERN = 0

	DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1"
	;colorbarSSTREN_gif, inc_T, imgcb_st, 0., LABELTHICKNESS = 1, SIZEOFLABEL = 2.0, COLOROFLABEL = 38+2, STRINGOFLABEL = '!3C', $
	;	BLACKCOLOR = 38+2, WHITECOLOR = 39+2;, TCOLORINC = Tcolorinc;, NUMCOLORS = numcolors, TCOLORMIN = Tcolormin
	
	;colorbarPHILCHLOR_gif, 0.
	;colorbarSSS_gif, POLECHK, 0.
	;colorbar_PHIL_SSS_gif, POLECHK, 0.
	;XYOUTS, 1.094,.0162, STRCOMPRESS('!10NASA!5', /REMOVE_ALL), $
	;    COLOR=202, /NORMAL,ALIGNMENT = .5
	;XYOUTS, 1.094,1.-m*.0162, STRMID(segs[n_segs-1],1,4), $
	;    COLOR=202, /NORMAL,ALIGNMENT = .5
	;PRINT, m,' ',STRMID(segs[n_segs-1],1,4)

	DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1"
	;XYOUTS, .94-.005-.013     , .965-.005D, 'psu', COLOR=202, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4
	;XYOUTS, .94+.032-.003, .96+.005D, '3', COLOR=34, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4
	;XYOUTS, .94, .96, 'mg/m!U3!N', COLOR=34, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4
	;XYOUTS, .94, .96, 'mg/m', COLOR=34, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4
	;XYOUTS, .935, .981-.005, 'SS Salinity', COLOR=202, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4
	;XYOUTS, .94, .97, 'Concentration', COLOR=34, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4
	DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--18-180-75-75-p-103-iso8859-1"
	DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1"
	;XYOUTS, .94, .96, 'ST', COLOR=BLACK, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.4

	;XYOUTS, 500.D/700.D, 202.D/600.D, '10N', COLOR=BLACK, /NORMAL, ALIGNMENT=.5, CHARSIZE=.9
	;XYOUTS, 500.D/700.D, 402.D/600.D, '15N', COLOR=BLACK, /NORMAL, ALIGNMENT=.5, CHARSIZE=.9
	;XYOUTS, 200.D/700.D, 101.D/600.D, '120E', COLOR=BLACK, /NORMAL, ALIGNMENT=0, CHARSIZE=.9
	;XYOUTS, 400.D/700.D, 101.D/600.D, '125E', COLOR=BLACK, /NORMAL, ALIGNMENT=0, CHARSIZE=.9
DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--14-100-100-100-p-82-iso8859-1"
;	XYOUTS, ncolXimg*(5.D/6.D)/(ncolXimg+xlen2add), nrowYimg*(1.D/3.D)/nrowYimg, '10N', COLOR=BLACK, /NORMAL, ALIGNMENT=.5, CHARSIZE=.9
;	XYOUTS, ncolXimg*(5.D/6.D)/(ncolXimg+xlen2add), nrowYimg*(2.D/3.D)/nrowYimg, '15N', COLOR=BLACK, /NORMAL, ALIGNMENT=.5, CHARSIZE=.9
;	XYOUTS, ncolXimg*(2.D/6.D)/(ncolXimg+xlen2add), nrowYimg*(1.D/6.D)/nrowYimg, '120E', COLOR=BLACK, /NORMAL, ALIGNMENT=0, CHARSIZE=.9
;	XYOUTS, ncolXimg*(4.D/6.D)/(ncolXimg+xlen2add), nrowYimg*(1.D/6.D)/nrowYimg, '125E', COLOR=BLACK, /NORMAL, ALIGNMENT=0, CHARSIZE=.9

	date$ = SYSTIME(0)
	PARSELINE, date$, ' ', dateseg$, ndseg
	date$ = dateseg$[1]+' '+dateseg$[2]+', '+dateseg$[4]
	DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--11-80-100-100-p-60-iso10646-1"
	DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--8-80-75-75-p-50-iso8859-1"
	XYOUTS, .931, .005, date$, COLOR=BLACK, /NORMAL, ALIGNMENT=.5, CHARSIZE=.9
	DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--10-100-75-75-p-60-iso8859-1"
	XYOUTS, .931, .936, imgdate$w, COLOR=BLACK, /NORMAL, ALIGNMENT=.5, CHARSIZE=.9
	
	DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--18-180-75-75-p-103-iso8859-1"
	;XYOUTS, .938, .03, imgdate$w, COLOR=199, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.3
	;XYOUTS, .73, .94, imgdate$w, COLOR=205, /NORMAL, ALIGNMENT=.5, CHARSIZE=1., CHARTHICK = 2.

	TVLCT,r2,g2,b2,/GET
	bmap=TVRD()
	;WRITE_GIF,outpath$+root$+'wDBOlocAve.gif',bmap,r2,g2,b2
	;PRINT,' ','Created: '+ outpath$+root$+'wDBOlocwklyAve.gif'
	
	fl2wrt = fl2wrt +'.gif'
	WRITE_GIF, fl2wrt, bmap, r2,g2,b2
	PRINT,'Created: '+fl2wrt
STOP

END

PRO READ_GLOBALGIF
READ_GIF, '/Volumes/KAB/GISS_d18OdD/Global_d18O_test/Gl_d18O_pt25deg.gif', image1, R, G, B
TVLCT, R, G, B
PRINT, R, G, B
TV, image1

STOP
END

