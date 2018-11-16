 

PRO latlonASIA_PHIL,ltmn,ltmx,ltinc,lnmn,lnmx,lninc,nc,nr,mag, BLACK

PLOT,[0,1],[0,1],XRANGE=[0,nc*mag],XSTYLE=5, $
  YRANGE=[0,nr*mag],YSTYLE=5,/NODATA,/noerase, $
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

for i=latmin,latmax,latinc do begin
  nn=0
  for j=lonmin,lonmax,loninc do begin
     rlat(nn)=DOUBLE(i)
     rlon(nn)=DOUBLE(j)
     nn=nn+1
  endfor
  ASIA_PHILlltoijarr,rlat,rlon,rcolidl,rrowidl
  rcoll=rcolidl(0:arrsize-3)
  rroww=rrowidl(0:arrsize-3)
  plots,rcoll*mag,rroww*mag,color=BLACK,NOCLIP=0,/data
endfor

arrsize2=FIX(((latmax-latmin)/latinc)+1+1)
rcoll=fltarr(arrsize2)
rroww=fltarr(arrsize2)
rlat=dblarr(arrsize2)
rlon=dblarr(arrsize2)
i=0.0D
j=0.0D
;loninc=5
ip = 0
for j=lonmin,lonmax,loninc do begin
  nn=0
  for i=latmin,latmax,latinc do begin
    rlat(nn)=DOUBLE(i)
    rlon(nn)=DOUBLE(j)
    nn=nn+1
  endfor
  ASIA_PHILlltoijarr,rlat,rlon,rcolidl,rrowidl
  rcoll=rcolidl(0:arrsize2-3)
  rroww=rrowidl(0:arrsize2-3)
  plots,rcoll*mag,rroww*mag,color=BLACK,NOCLIP=0,/data
;
ip=ip+1
endfor

end

PRO ASIA_PHILlltoijarr, lat, lon, ix, jy ; X,Y, COL, ROW

	; THIS IS FOR PHILIPEANS ONLY 
	; uintPHIL = avhrrch4(xo:xo+ncol_img,yo:yo+nrow_img)
	; xo = 282
	; yo = 445
	; nrow=500L;y
	; ncol=445L;x
	; nrow_avhr =nrow*2
	; ncol_avhr =ncol*2
	; nrow_img = nrow*2
	; ncol_img = ncol*2
	; lm_mapPHIL = lm_map_12_5(xo:xo+ncol_img,yo:yo+nrow_img)

	;/****local declarations struct xypair lltoxyARL(double lat, double lon) ********/
	GRIDLENGTHY = (2225.D)
	GRIDLENGTHX = (2500.D)
	GRIDSECTION = (5.D)
	xdist = GRIDLENGTHX;
	ydist = GRIDLENGTHY;

	Cire = (40075.D);	/* km */
	Cirp = (40007.D);	/* km */
			;latctr = (5.D);	/* deg */
			;lonctr = (122.5D);	/* deg */
	latctr = (10.D);	/* deg */
	lonctr = (117.5D);	/* deg */
	x = (0.D);
	y = (0.D);

	;/****************************** end ***************************************/

	;/* compute  x and y distance from the pole */

     	 y = (lat - latctr)*Cirp/(360.D);  
     	 x = (lon - lonctr)*Cire/(360.D);  

	;/* find the i, j pair convert distances from pole to I,J pair */
	ix = ROUND(((x + xdist - GRIDSECTION/2.D) / GRIDSECTION));
	jy = ROUND(((y + ydist - GRIDSECTION/2.D) / GRIDSECTION));

END 

PRO HIMA2_lltoijarr, lat, lon, ix, jy

	; THIS IS FOR Himalaya Range ONLY 
	; uintPHIL = avhrrch4(xo:xo+ncol_img,yo:yo+nrow_img)
	; xo = 
	; yo = 
	; nrow=500L+100
	; ncol=750L+200
	; nrow_avhr =nrow*2
	; ncol_avhr =ncol*2
	; nrow_img = 500L
	; ncol_img = 750L
	; lm_mapPHIL = lm_map_12_5(xo:xo+ncol_img,yo:yo+nrow_img)
	Cire = DOUBLE(40075.D);	/* km */
	Cirp = DOUBLE(40007.D);	/* km */
	;PRINT, (Cirp/DOUBLE(360.D))*11./(5.);=244.48722
	;PRINT, (Cirp/DOUBLE(360.D))*10./(5.);=222.26111
	;PRINT, (Cire/DOUBLE(360.D))*15./(5.);=333.95833
	;PRINT, (Cire/DOUBLE(360.D))*18./(5.);=400.75000

    ;PRINT, (Cirp/DOUBLE(360.D))*5.D /(5.D);=111.13056
    ;PRINT, (Cirp/DOUBLE(360.D))*15.D /(5.D);=333.39167
    ;PRINT, (Cire/DOUBLE(360.D))*25.D /(5.D);=556.59722
	
	;/****local declarations struct xypair lltoxy(double lat, double lon) ********/
	GRIDSECTION = (5.D)
	GRIDLENGTHY = (250.D*5.D)
	;GRIDLENGTHX = (350.D*5.D)
	GRIDLENGTHX = (550.D*5.D)
	xdist = GRIDLENGTHX;
	ydist = GRIDLENGTHY;

	latctr = (35.D);   /* deg */
	lonctr = (90.D);   /* deg */
	x = DOUBLE(0.D);
	y = DOUBLE(0.D);

	;/****************************** end ***************************************/

	;/* compute  x and y distance from the pole */

     	 y = (lat - latctr)*Cirp/(360.D);  
     	 x = (lon - lonctr)*Cire/(360.D);  

	;/* find the i, j pair convert distances from pole to I,J pair */
	ix = ROUND(((x + xdist - GRIDSECTION/2.D) / GRIDSECTION));
	jy = ROUND(((y + ydist - GRIDSECTION/2.D) / GRIDSECTION));

END 

PRO latlonHIMA,ltmn,ltmx,ltinc,lnmn,lnmx,lninc,nc,nr,mag, BLACK

PLOT,[0,1],[0,1],XRANGE=[0,nc*mag],XSTYLE=5, $
  YRANGE=[0,nr*mag],YSTYLE=5,/NODATA,/noerase, $
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

for i=latmin,latmax,latinc do begin
  nn=0
  for j=lonmin,lonmax,loninc do begin
     rlat(nn)=DOUBLE(i)
     rlon(nn)=DOUBLE(j)
     nn=nn+1
  endfor
  HIMA2_lltoijarr,rlat,rlon,rcolidl,rrowidl
  rcoll=rcolidl(0:arrsize-3)
  rroww=rrowidl(0:arrsize-3)
  plots,rcoll*mag,rroww*mag,color=BLACK,NOCLIP=0,/data
endfor

arrsize2=FIX(((latmax-latmin)/latinc)+1+1)
rcoll=fltarr(arrsize2)
rroww=fltarr(arrsize2)
i=0.0D
j=0.0D
;loninc=5
ip = 0
for j=lonmin,lonmax,loninc do begin
  nn=0
  for i=latmin,latmax,latinc do begin
    rlat(nn)=DOUBLE(i)
    rlon(nn)=DOUBLE(j)
    nn=nn+1
  endfor
  HIMA2_lltoijarr,rlat,rlon,rcolidl,rrowidl
  rcoll=rcolidl(0:arrsize2-3)
  rroww=rrowidl(0:arrsize2-3)
  plots,rcoll*mag,rroww*mag,color=BLACK,NOCLIP=0,/data
;
ip=ip+1
endfor

end
