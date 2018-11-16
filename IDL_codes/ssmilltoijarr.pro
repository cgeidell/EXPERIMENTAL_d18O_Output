pro ssmilltoijarr,rlat,rlong,cip,cjp,polp,longtype$,freqch
;
;
; S. Fiegles 3/18/96
; IDL array processing version
;
; cip = row value
; cjp = column value
; polp         [+1=north      -1=south]
; longtype$ 'E' for EAST longitude  'W' west longitude
; freqch  either 85 for 85 GHZ data or (any other number for 25 km. data)
;
;
;  (columns,rows) are in a (0,0) based coordinate array system
; with (0,0) being the upper left corner of the upper left grid cell
;   
; Since the pole is at the intersection of 4 grid cells the 
; integerized floating point number (154.0,234.0) occurs in 
; the upper left corner of a gridcell in  the (0,0) based coordinate array system
; Thus the actual center of a grid cell is an added value of 0.5
; (154.5,234.5)
;
; upper left corner ( colval+0.0,rowval+0.0)
;    grid cell center              ( colval+0.5,rowval+0.5)
; lower right corner                            ( colval+1.0,rowval+1.0)
;

; In order to obtain the proper integer cartesian coordinates (col,row)
; the floating point values returned must be TRUNCATED not rounded
;
;   polp = +1 North       -1=South
;
;
;
; polp         [+1=north      -1=south]
;
; west to east long conversion
;
if(freqch NE 85)then gridsize=25.0D0
if(freqch EQ 25)then gridsize=25.0D0
if(freqch EQ 85)then gridsize=12.5D0
if(freqch EQ 12)then gridsize=12.5D0
if(freqch EQ 6)then gridsize=6.25D0
if(freqch EQ 3)then gridsize=3.125D0
if(freqch EQ 1)then gridsize=1.5625D0
if(polp NE 1) then polp= DOUBLE(-1.0D0)
 rlat=DOUBLE(rlat)
 rlong=DOUBLE(rlong)
if(STRUPCASE(longtype$) EQ  'W') then  rlong=360.0D0 -rlong
;
slat=70.0D0     ; standard latitude for ssm/i
re=6378.273D   ; radius of earth
e2=0.006693883D ; eccentricity of earth squared
pi4=DOUBLE(!DPI/4.0D0)
e=0.0D0 ; eccentricity
cdr= DOUBLE(180.0D0/!DPI)
delta=0.0D0 ; meridian offset for north pole=45 (sp=> delta=0)
sgn=0.0D0   ; northern hemisphere = +1 southern hemisphere= -1)
xdist=rlat * (0.0D)
ydist=rlat * (0.0D)
x=rlat
y=rlat
x(*)=0.0D
y(*)=0.0D
t=rlat
t(*)=0.0D0
tc=t
mc=t
rho=t
slat=t
slat(*)=70.0D0     ; standard latitude for ssm/i
;xdist=0.0D0     ;x location km of upper left corner(np=3850) (sp=3950)
;ydist=0.0D0     ;y location km of upper left corner(np=5850) (sp=4350)
 ;  sl=0.0D0 & 0 & mc=0.0D0 & lat=0.0D0 
;lon=0.0D0 & lattmp=0.0D0 & lontmp=0.0D0 & cm=0.0D0
;
;      sl=slat/cdr
;     tc=TAN(0.785399- sl/2.0D0) / $
;        (  ( (1.0D0-e*SIN(sl))/(1.0D0+e*SIN(sl))) ^ (e/2.0D0) )
;       mc= ( COS(sl) / SQRT(1.0D0 - e2*SIN(sl)*SIN(sl)))
sgn=DOUBLE(polp)
if(sgn EQ 1.0D0) then BEGIN
delta=45.0D0 & xdist(*)=3850.0D0 & ydist(*)=5850.0D0
ENDIF ELSE BEGIN
delta=0.0D0 & xdist(*)=3950.0D0 & ydist(*)=4350.D
ENDELSE
e=SQRT(e2)
; lat lon from degrees to radians
lat= ABS(rlat)/ cdr
lon= ABS(rlong+delta)/ cdr
hl=(WHERE(( lat GT 89.995D) OR (lat LT -89.995D)))
ll=WHERE(( lat LE 89.995D) OR (lat GE -89.995D))
;***************************************************************
if(ll(0) NE -1L) then BEGIN
llat=lat(ll)
t(ll)= $
  TAN(0.785399D0- (llat/2.0D0))/ ((1.0D0-e*SIN(llat)) / (1.0D0+e*SIN(llat))) ^ (e/2.0D0)
;***********************************************************************
;***********************************************************************
lat2=WHERE((( rlat LE 89.995D) OR (rlat GE -89.995D)) AND $
            (ABS(90.0D0-slat) LT 1.0D0-5))
if(lat2(0) NE -1L) then BEGIN
 rho(lat2)=2.0D0*re*t(lat2)/(( ((1.0D0+e)^(1.0D0+e)) * ((1.0D0-e)^(1.0D0-e)) )^ (e/2.0D0) )
               ENDIF
;*************************************************************************
lat3=WHERE((( rlat LE 89.995D) OR (rlat GE -89.995D)) AND $
            (ABS(90.0D0-slat) GE 1.0D0-5))
if(lat3(0) NE -1L) then BEGIN
      sl=slat/cdr
      tc(lat3)=TAN(0.785399D0- sl/2.0D0) / $
         (  ( (1.0D0-e*SIN(sl))/(1.0D0+e*SIN(sl))) ^ (e/2.0D0) )
        mc=DOUBLE( ( COS(sl) / SQRT(1.0D0 - e2*SIN(sl)*SIN(sl))))

       rho(lat3)=re * mc * t(ll)/tc(lat3)
ENDIF
;****************************************************************************
  y(ll)= (-1.0D0)* rho * sgn *cos(sgn*lon(ll))
  x(ll)= rho * sgn * SIN(sgn*lon(ll))


ENDIF ; end of llat
;**********************************************************
if(hl(0) NE -1L) then BEGIN
hlat=lat(hl)
x(hl)=0.0D0  
y(hl)=0.0D0
ENDIF ; end of hlat
;
; grid coords dist from pole to i,j pair
;
; convert distances from pole to i,j pair

cjp=(((x+xdist) /gridsize) * 1.0D0)
cip=(((ydist-y) /gridsize) *1.0D0)
; convert back from  east to west
if(STRUPCASE(longtype$) EQ  'W') then  rlong=360.0D0 -rlong
;print,'lat=',rlat,' long=',rlong,' x=',x,' y=',y,' cip(row)=',cip,$
;               '  cjp (col)=',cjp,' col=',FIX(cjp),' row=',FIX(cip)
END
