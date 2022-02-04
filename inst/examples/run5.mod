;****************************************************************************************************************
$PROB RUN5
$INPUT ID TIME DV MDV AMT EVID CMT
;****************************************************************************************************************
$DATA datasim.csv IGNORE=@
;****************************************************************************************************************
$SUBROUTINE ADVAN8 TOL=5
$MODEL 
 COMP(ABSORB,DEFDOSE)
 COMP (CENTRAL)
 COMP (PERIPH)

$PK

 TVV2  = THETA(1)
 TVCL  = THETA(2)
 
 V2=TVV2*EXP(ETA(1))                                                 ; volume central
 CL=TVCL*EXP(ETA(2))                                                 ; clearance
 V3=THETA(3)  									                     ; volume peripheral
 Q =THETA(4)  									                     ; intercompartimental clearance 
 KA=THETA(5)			        									 ; absorption rate constant
 
 K20=CL/V2
 K23=Q/V2
 K32=Q/V3
 
;****************************************************************************************************************
$DES
 DADT(1) = -A(1)*KA                              ; First compartment is absorption compartment
 DADT(2) = A(1)*KA + A(3)*K32 -A(2)*(K20+K23)    ; Central Compartment
 DADT(3) = A(2)*K23-A(3)*K32                     ; Peripheral Compartment   

;****************************************************************************************************************
$ERROR
IPRD1=A(2)/V2
IPRED= IPRD1
PROP=THETA(6)*IPRD1  
W=SQRT(PROP**2)
IRES=DV-IPRED
IWRES=IRES/W
Y= IPRED + W*EPS(1)

;****************************************************************************************************************
$THETA  (0,20)   ; V2, label='Central Volume', units='L', trans='', type='Structural'
$THETA  (0,0.6)  ; CL, label='Clearance', units='L/h', trans='', type= 'Structural'
$THETA  (0,80)   ; V3, label='Peripheral Volume', units='L', trans='', type='Structural'
$THETA  (0,1.8)  ; Q,  label='Intercompartimental Clearance', units='L/h', trans='', type='Structural'
$THETA  (0,0.7)  ; KA, label='Absorption Rate', units='h-1', trans='', type='Structural'
$THETA  (0,0.1)  ; ERRPROP, label='Proportional Error', units='-', trans='', type='RUV'

$OMEGA BLOCK(2)
0.1         ;nV2, label='On Volume', type='IIV'
0.02 0.1    ;nCL, label='On Clearance', type='IIV'

;Initial value of SIGMA
$SIGMA 
1 FIX       

$ESTIMATION MAXEVALS=9999 METHOD=1 INTER NOABORT NSIG=3 POSTHOC PRINT=1    
$COVARIANCE UNCONDITIONAL   