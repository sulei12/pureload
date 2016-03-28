
    SUBROUTINE F1A_PRE(I,J,TAO,P,ID)
    USE PARAMETEERS
    USE ROTOR
    IMPLICIT NONE
    INTEGER,INTENT(IN)::I,J,ID
    REAL(8),INTENT(IN)::TAO
    REAL(8),INTENT(OUT)::P
    
    REAL(8)::TEMP,PSI
    
    REAL(8)::RTR(3),VEL(3),ACC(3),LOAD(3),DET_LOAD(3)
    REAL(8)::MACH_R,MACH,RTR1,MACH_I(3)
    REAL(8)::RTR_I(3)
    REAL(8)::VEL_N,ACC_N,LOAD_N,DET_LOAD_N
    REAL(8)::PT,PL
    
    REAL(8)::NPANEL(3),SPANEL
    REAL(8)::ANGLE
    
    !
    !P=0
    !IF (ID.EQ.1)THEN
    !NPANEL=NPANEL_UP(:,I,J)
    !SPANEL=SPANEL_UP(I,J)
    !ELSE IF(ID.EQ.2)THEN
    !NPANEL=NPANEL_DOWN(:,I,J)
    !SPANEL=SPANEL_DOWN(I,J)
    !END IF
    !
    !
    !PSI=TAO*OMEGA
    !CALL UPDATA_PANEL_VEL(I,J,ID,PSI,VEL,ACC,LOAD,DET_LOAD)
    
   ! LOAD=LOAD*100
    
    !RTR1=SQRT(SUM(RTR*RTR))
    !MACH_I=VEL/SOUNDSPEED
    !MACH_R=DOT_PRODUCT(VEL,RTR/RTR)/SOUNDSPEED
    !MACH=SQRT(SUM(VEL*VEL))/SOUNDSPEED
    !!
    !VEL_N=DOT_PRODUCT(VEL,NPANEL)
    !ACC_N=DOT_PRODUCT(ACC,NPANEL)
    !
    !RTR_I=RTR/RTR1
    !
    !ANGLE=ACOS(DOT_PRODUCT(NPANEL,RTR)/RTR1)
    !LOAD_N=DOT_PRODUCT(LOAD,NPANEL)
    !DET_LOAD_N=DOT_PRODUCT(DET_LOAD,NPANEL)
    !
    !
    !PT= (RHO*ACC_N)/(RTR1*(1-MACH_R)*(1-MACH_R))+    &
    !    RHO*VEL_N*DOT_PRODUCT(RTR_I,ACC)/SOUNDSPEED+    &
    !    RHO*SOUNDSPEED*VEL_N*(MACH_R-MACH*MACH)/(RTR1*RTR1*(1-MACH_R)*(1-MACH_R)*(1-MACH_R) ) 
    !PT=PT*SPANEL/(4*PI)
    !!PL=(DET_LOAD*COS(ANGLE)/(SOUNDSPEED*RTR1*(1-MACH_R)*(1-MACH_R))+(DOT_PRODUCT(RTR_I,ACC))*LOAD_N*COS(ANGLE)/(SOUNDSPEED*RTR1*(1-MACH_R)*(1-MACH_R)*(1-MACH_R))+(LOAD_N*(COS(ANGLE)-DOT_PRODUCT(VEL/SOUNDSPEED,NPANEL))/(RTR1*RTR1*(1-MACH_R)*(1-MACH_R) ) ) +(((MACH_R-MACH*MACH)*LOAD_N*COS(ANGLE) )/(RTR1*RTR1*(1-MACH_R)*(1-MACH_R)*(1-MACH_R)))   )
    !
    !PL= DOT_PRODUCT(DET_LOAD,RTR_I)/(SOUNDSPEED*RTR1*(1-MACH_R)*(1-MACH_R))+    &
    !    (LOAD_N-DOT_PRODUCT(LOAD,MACH_I))/(RTR1*RTR1*(1-MACH_R)*(1-MACH_R) )  + &
    !    (DOT_PRODUCT(RTR_I,ACC))*LOAD_N/(SOUNDSPEED*RTR1*(1-MACH_R)*(1-MACH_R)*(1-MACH_R))+ &
    !    (((MACH_R-MACH*MACH)*LOAD_N*COS(ANGLE) )/(RTR1*RTR1*(1-MACH_R)*(1-MACH_R)*(1-MACH_R)))   
    !PL=PL*SPANEL/(4*PI)
    !
    !P=PT+PL
    

    
    END SUBROUTINE F1A_PRE
    
   
    