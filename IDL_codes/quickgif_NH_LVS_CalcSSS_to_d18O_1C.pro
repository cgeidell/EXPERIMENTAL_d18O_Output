@/Volumes/KAB/bin/MkGACgifs/latlonGAC_wlC.pro
@/Volumes/KAB/bin/colorbar_avhrr1B.pro
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

;maxcb = 5.D
;mincb = -15.D

maxcb = 1.5
mincb = -6.D

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

	lblV = [.01D,.02,.03,.05,.1,.2,.3,.5,1,2,3,5D,10,20,30,50D]
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

maxcb = 1.5
mincb = -6.D

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
	;	 XYOUTS, xst-.007D, yst-.005D, STRCOMPRESS(STRING(FORMAT='(f5.2)', TPR), /REMOVE_ALL), /NORMAL $
	;	 , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
	;    ENDIF ELSE IF (lblV[m] LT 1.) THEN BEGIN
	;	 XYOUTS, xst-.007D, yst-.005D, STRCOMPRESS(STRING(FORMAT='(f5.1)', TPR), /REMOVE_ALL), /NORMAL $
	;	 , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
	;    ENDIF ELSE BEGIN
	;	 XYOUTS, xst-.007D, yst-.005D, STRCOMPRESS(STRING(FORMAT='(f5.0)', TPR), /REMOVE_ALL), /NORMAL $
	;	 , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
	    ;ENDELSE
		 XYOUTS, xst-.007D, yst-.005D, STRCOMPRESS(STRING(FORMAT='(f5.0)', TPR), /REMOVE_ALL), /NORMAL $
		 , ALIGNMENT = 1., SIZE = 1.6, COLOR = BLACKc+cb_skip
	ENDFOR
	;XYOUTS, .2, .5, 'Chlorophyll Concentration, OC4 Algorithm' $
	;, /NORMAL, ALIGNMENT = .5, SIZE = 1.2, COLOR = BLACKc+cb_skip, ORIENTATION = 90.
	;XYOUTS, .4, .5, 'mg/m!U3!N', /NORMAL, ALIGNMENT = .5, SIZE = 1., COLOR = BLACKc+cb_skip $
	;, ORIENTATION = 90.


END


PRO calcSSS_to_d18O

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

 ; IC SECTORS
locnames = ['a) N. Hemisphere', 'b) Arctic Ocean', 'c) Greenland Sea', 'd) Kara/Barents Seas', 'e) Bering Sea', 'f) Okhotsk/Japan Seas', $
 'g) Canadian Archipelago', 'h) Baffin Bay/Labrador Sea', 'i) Hudson Bay', 'j) Gulf of St Lawrence']
datarowpick = [11, 8, 6, 7, 3, 2, 9, 5, 4, 10]-2


	;icSector25Mask = INTARR(ncol, nrow) 
	;GET_LUN, filen
	;OPENR, filen, '/Volumes/KAB/bin//np_sector.msk', /SWAP_IF_LITTLE_ENDIAN
	;;OPENR, filen, fn, /SWAP_IF_LITTLE_ENDIAN
	;READU, filen, icSector25Mask
	;FREE_LUN, filen
	;icSector25Mask = ROTATE(TEMPORARY(icSector25Mask), 7)
	;GET_LUN, filen
	;WINDOW, 0, xsize = ncol_img, ysize = nrow_img
	;TVSCL, icSector25Mask

	;fn = '/Volumes/KAB/bin/bin/np_sector.new_12km'
	GET_LUN, filen
	sectormsk = INTARR(ncol_img, nrow_img) 
	OPENR, filen, '/Volumes/KAB/np_sector.new_12km'
	READU, filen, sectormsk
	FREE_LUN, filen
	sectormsk = ROTATE(TEMPORARY(sectormsk), 7)
	WINDOW, 0, COLORS = 256, RETAIN = 2
	TVSCL, sectormsk*20
	
	; 2 Okhotsk/Japan Seas
	; 3 Bering Sea
	; 4 Hudson Bay
	; 5 Baffin Bay - Davis Straight - Labrador Sea
	; 6 Greenland Sea
	; 7 Kara Sea - Barents Seas
	; 8 Arctic ocean
	; 9 Canadian Archipelago
	; 10 Gulf of St. Lawrence 
	; 11 LAND
	; 12 COAST
	; 13 Norway Straights
	; 14 West of 2 
	
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


	;datINT = INTARR(ncol_img, nrow_img) ;For INTS - Monthlies 
  datINT = DBLARR(ncol_img, nrow_img) ;For DBLS - Seasonal 
	datSSS = DBLARR(ncol_img, nrow_img)
	imgflt = DBLARR(ncol_img, nrow_img)
	
	flsnames = FILE_SEARCH('/Volumes/KAB/Climatological_SSS_NH/SESN_CLIM/sh_*_*_SESN_CLIM_AqGSFCv5_SH_12_5kmDBL_AscDsc.dat.gz', COUNT = nfls)
  ;flsnames = FILE_SEARCH('/Users/CGE/Desktop/d18O_Project/Aquarius_NH_Monthlies/nh_*_SSS_v04_SH_12_5kmINT.dat.gz', COUNT = nfls)
	;/Volumes/NASA/NH_Aq_Monthlies/nh_2011_08_SSS_v04_SH_12_5kmINT.dat.gz
	;/Users/CGE/Desktop/d18O_Project/Aquarius_NH_Monthlies

	;fn = '/Volumes/KAB/bin/SSS/Aquarius_NH_Monthlies/nh_2012_09_SSS_v04_SH_12_5kmINT.dat.gz'
	
	FOR ifl = 0L, nfls-1 DO BEGIN
		fn = flsnames[ifl]
		OPENR, 5, fn, /COMPRESS
		READU, 5, datINT
		CLOSE, 5
		datSSS[*,*] = -9999.D
		wgSSS = WHERE(datINT GT 0, cntgSSS)
		datSSS[wgSSS] = DOUBLE(datINT[wgSSS]) ;For when data are DOUBLES
		;datSSS[wgSSS] = DOUBLE(datINT[wgSSS])/100.D ;For when data are INTEGERS 
		PRINT, MAX(datINT), MIN(datINT)
		PRINT, MAX(datSSS), MIN(datSSS)
		;WINDOW, 1, xsize = ncol_img, ysize = nrow_img, COLORS=256
		;TVSCL, datSSS

  ;STOP 
  	
	wareaA = WHERE((datINT GT 0) AND (sectormsk EQ 8 OR sectormsk EQ 9 OR sectormsk EQ 7), cntA)
	wareaB = WHERE((datINT GT 0) AND (sectormsk EQ 6 OR sectormsk EQ 13), cntB)
	wareaC = WHERE((datINT GT 0) AND (sectormsk EQ 5) AND (map_lat GE 62.D), cntC)
	wareaD = WHERE((datINT GT 0) AND (sectormsk EQ 5) AND (map_lat LT 62.D), cntD)
	wareaE = WHERE((datINT GT 0) AND (sectormsk EQ 4), cntE)
	wareaF = WHERE((datINT GT 0) AND (sectormsk EQ 1) AND ((map_lon LT 90.D) OR (map_lon GT 270.D)), cntD)
	wareaG = WHERE((datINT GT 0) AND (sectormsk EQ 10), cntB)
	wareaH = WHERE((datINT GT 0) AND (sectormsk EQ 1 OR sectormsk EQ 14) AND (map_lon GT 90.D) AND (map_lon LT 270.D), cntD)
	wareaI = WHERE((datINT GT 0) AND (sectormsk EQ 2 OR sectormsk EQ 3), cntI)

		datd18O = datSSS
		datd18O[wareaA] = datSSS[wareaA]*0.48 -16.82
		datd18O[wareaB] = datSSS[wareaB]*0.60 -20.71
		datd18O[wareaC] = datSSS[wareaC]*0.33 -11.82
		datd18O[wareaD] = datSSS[wareaD]*0.94 -32.45
		datd18O[wareaE] = datSSS[wareaE]*0.42 -16.05
		datd18O[wareaF] = datSSS[wareaF]*0.55 -18.98
		datd18O[wareaG] = datSSS[wareaG]*0.55 -18.98
		datd18O[wareaH] = datSSS[wareaH]*0.44 -15.13
		datd18O[wareaI] = datSSS[wareaI]*0.44 -15.13
		
		WINDOW, 2, xsize = ncol_img, ysize = nrow_img, COLORS=256
		TVSCL, datd18O
		PRINT, 'MAX(datd18O)', MAX(datd18O)
		PRINT, 'MIN(datd18O)', MIN(datd18O)

		;outpath$ = '/Volumes/NASA/'+ pole +'H_d18O_TEST/'
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

		;maxcb = 5.D
		;mincb = -15.D

		maxcb = 1.5
		mincb = -6.D

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
	;	XYOUTS, .94, .005, date$, COLOR=202, /NORMAL, ALIGNMENT=.5, CHARSIZE=.9
		DEVICE, SET_FONT = "-adobe-helvetica-bold-r-normal--18-180-75-75-p-103-iso8859-1"
		;XYOUTS, .938, .03, imgdate$w, COLOR=199, /NORMAL, ALIGNMENT=.5, CHARSIZE=1.3
	;	XYOUTS, .6, .94, imgdate$w, COLOR=205, /NORMAL, ALIGNMENT=.5, CHARSIZE=1., CHARTHICK = 2.
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

