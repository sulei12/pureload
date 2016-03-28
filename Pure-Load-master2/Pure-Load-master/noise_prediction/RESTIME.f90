
    SUBROUTINE RESTIME(I_R,J_C,TAO,IK,IB,PT,PL,PT_NEAR,PT_FAR,PL_NEAR,PL_FAR)
    !!求延迟时间TAO
    !IK:1 FOR UP SURFACE,2 FOR DOWN SURFACE
    USE ROTOR
    USE COOR_TRAN
    USE GLOBAL_PARAMETERS
    USE PARAMETEERS
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: I_R,J_C,IK,IB
    REAL(8),INTENT(OUT) :: TAO,PT,PL,PT_NEAR,PT_FAR,PL_NEAR,PL_FAR
    
    REAL(8)::PT1,PT2,PT3,PL1,PL2,PL3,PL4
    
    INTEGER :: I,J,K,ITMAX=300,ITEM=0
    REAL(8) :: TAO_PSI,TEMP,TEMP3(3)
    REAL(8) :: TAO_UPPER,TAO_DOWN,FUNC
    REAL(8) :: EPS
    REAL(8) :: BETA_RES,THETA_RES,SPRESS,TAO_PSI_deg

    REAL(8)::RTR(3),VEL(3),ACC(3),LOAD(3),LOAD_DOT(3),VEL_N_DOT,MACH_I_DOT(3)
    REAL(8)::MACH_R,MACH,RTR1,MACH_I(3)
    REAL(8)::RTR_I(3)
    REAL(8)::VEL_N,LOAD_R,LOAD_PRESSURE,LOAD_PRESSURE_DOT
    !REAL(8)::PT,PL
    
    REAL(8)::XPANEL(3),NPANEL(3),SPANEL
    REAL(8)::npanel_XYZ5(3),cs_theta,NPANEL_0_DOT(3)
    
    
    
    
        IF (IK.EQ.1)THEN
            XPANEL=XPANEL_UP(:,I_R,J_C)
            NPANEL=NPANEL_UP(:,I_R,J_C)
            SPANEL=SPANEL_UP(I_R,J_C)
        ELSE IF(IK.EQ.2)THEN
            XPANEL=XPANEL_DOWN(:,I_R,J_C)
            NPANEL=NPANEL_DOWN(:,I_R,J_C)
            SPANEL=SPANEL_DOWN(I_R,J_C)
        END IF
      !  XPANEL=0.5*(XPANEL_UP(:,I_R,J_C)+XPANEL_DOWN(:,I_R,J_C))
      !  NPANEL=0.5*(NPANEL_UP(:,I_R,J_C)+NPANEL_DOWN(:,I_R,J_C))
    
      
    EPS=MIN(1E-6,C_BLADE_R(1)/N_C/OMEGA/R_BLADE_END)
    TAO_UPPER=T_VISIT
    TAO_DOWN=-10.0

    DO I=1,ITMAX
        TAO=(TAO_DOWN+TAO_UPPER)/2
        TAO_PSI=OMEGA*TAO
        TAO_PSI=TAO_PSI+BLADE_SPACING(IB)*PI/180
        TAO_PSI_deg=TAO_PSI*180/pi
        
        CALL UPDATE_STAT(IB,XPANEL(1),TAO_PSI,BETA_RES,THETA_RES)
        CALL TRAN_A21(AL_ROTOR,A21)
        CALL TRAN_A32(TAO_PSI,A32)
        CALL TRAN_A43(BETA_RES,A43)
        CALL TRAN_A54(THETA_RES,A54)
        A51=MATMUL(A54,MATMUL(A43,matmul(A32,A21)))
        A15=TRANSPOSE(A51)
        !TEMP3=MATMUL(TRANSPOSE(A54),XPANEL_UP(:,I_R,J_C))
        !TEMP3=MATMUL(A15,XPANEL_UP(:,I_R,J_C))
        !CALL TRAN_A15(AL_ROTOR,TAO_PSI,BETA_RES,THETA_RES,A15)
        
        FUNC=T_VISIT-TAO-SQRT(SUM((MATMUL(A15,XPANEL)-X_VISIT)*(MATMUL(A15,XPANEL)-X_VISIT)))/SOUNDSPEED

        IF (FUNC .GT. EPS) THEN  
            TAO_DOWN =TAO
        ELSE IF (FUNC .LT. -EPS)THEN
            TAO_UPPER =TAO
        ELSE
            EXIT
        END IF
    END DO
    !------------------------------------------------------------------------------------------------------------
    !------------------------------------------------------------------------------------------------------------
        
     

        
        
        
        CALL UPDATE_STAT(IB,XPANEL(1),TAO_PSI,BETA_RES,THETA_RES)
        !CALL UPDATA_PANEL_VEL(IB,I_R,J_C,IK,TAO_PSI,BETA_RES,THETA_RES,VEL,VEL_N,VEL_N_DOT,MACH_I,MACH_I_DOT,LOAD,LOAD_DOT,LOAD_PRESSURE,LOAD_PRESSURE_DOT)
        CALL UPDATA_PANEL_VEL(IB,I_R,J_C,IK,TAO_PSI,BETA_RES,THETA_RES,VEL,VEL_N,VEL_N_DOT,MACH_I,MACH_I_DOT,LOAD,LOAD_DOT,LOAD_PRESSURE,LOAD_PRESSURE_DOT)
        CALL TRAN_A15(AL_ROTOR,TAO_PSI,BETA_RES,THETA_RES,A15)
        NPANEL_XYZ5=MATMUL(A15,NPANEL)
        
        RTR=X_VISIT-(MATMUL(A15,XPANEL))
        RTR1=SQRT(SUM(RTR*RTR))
        RTR_I=RTR/RTR1
        MACH_R=DOT_PRODUCT(MACH_I,RTR_I)
        MACH=SQRT(DOT_PRODUCT(MACH_I,MACH_I))
        
 
        PT1= (RHO*VEL_N_DOT)/(RTR1*(1-MACH_R)*(1-MACH_R))
        PT2= RHO*VEL_N*DOT_PRODUCT(RTR_I,MACH_I_DOT)/(RTR1*(1-MACH_R)*(1-MACH_R)*(1-MACH_R))
        PT3= RHO*SOUNDSPEED*VEL_N*(MACH_R-MACH*MACH)/(RTR1*RTR1*(1-MACH_R)*(1-MACH_R)*(1-MACH_R))
        PT1=PT1*SPANEL/(4*PI)
        PT2=PT2*SPANEL/(4*PI)
        PT3=PT3*SPANEL/(4*PI)
        PT_NEAR=PT3
        PT_FAR=PT1+PT2
        PT=PT_NEAR+PT_FAR
        
        

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         LOAD_R=DOT_PRODUCT(LOAD,RTR_I)
         TEMP=DOT_PRODUCT(LOAD_DOT,RTR_I)
         !LOAD_DOT=0
         !LOAD_DOT=0
         !考虑粘性力产生影响
         PL1=DOT_PRODUCT(LOAD_DOT,RTR_I)/(SOUNDSPEED*RTR1*(1-MACH_R)*(1-MACH_R))
         PL3=LOAD_R*(dot_product(MACH_I_DOT,rtr_i))/(SOUNDSPEED*RTR1*(1-MACH_R)*(1-MACH_R)*(1-MACH_R))
         PL2=(LOAD_R-DOT_PRODUCT(LOAD,MACH_I))/(RTR1*RTR1*(1-MACH_R)*(1-MACH_R) )
         PL4=(((MACH_R-MACH*MACH)*LOAD_R)/(RTR1*RTR1*(1-MACH_R)*(1-MACH_R)*(1-MACH_R))) 
        
         !LOADING只考虑法向力
         !这两种有一些微小差别。文献中用这两种的情况都有。折合成分贝值计算在0.5db左右
        !cs_theta=dot_product(NPANEL_XYZ5,RTR_I)/SQRT(DOT_PRODUCT(NPANEL_XYZ5,NPANEL_XYZ5))
        !PL1=(LOAD_PRESSURE_DOT)/(SOUNDSPEED*RTR1*(1-MACH_R)*(1-MACH_R))
        !PL3=LOAD_PRESSURE*cs_theta*(dot_product(rtr_i,MACH_I_DOT))/(SOUNDSPEED*RTR1*(1-MACH_R)*(1-MACH_R)*(1-MACH_R))
        !PL2=(LOAD_PRESSURE*(cs_theta-dot_product(Mach_i,NPANEL_XYZ5)) )/(RTR1*RTR1*(1-MACH_R)*(1-MACH_R) )
        !PL4=(MACH_R-MACH*MACH)*LOAD_PRESSURE*CS_THETA/(RTR1*RTR1*(1-MACH_R)*(1-MACH_R)*(1-MACH_R))
        
        PL1=PL1*SPANEL/(4*PI)
        PL2=PL2*SPANEL/(4*PI)
        PL3=PL3*SPANEL/(4*PI)
        PL4=PL4*SPANEL/(4*PI)
        PL_FAR=pl1+pl3
        PL_NEAR=pl2+pl4
        PL=PL_FAR+PL_NEAR
        SPRESS=PT+PL
        
        
        
    
1000 FORMAT(7(F12.6,2X))
2000 FORMAT(2I4,5F12.6)
3000 FORMAT('OBTIME:',F12.6,5x,'RESTIME:',F12.6,5x,'PSI:',F12.6,5X,'BETA_DEG:',F12.6,5X,'THETA_DEG:',F12.6,5X,'RTR1',F12.6,5X,'VEL_N',F12.6,5X,'PT:',F15.8,5X,'PT1:',F15.8,5X,'PT2:',F15.8,5X,'PT3:',F15.8,5X)     
4000 FORMAT('OBTIME:',F12.6,5x,'RESTIME:',F12.6,5x,'PSI:',F12.6,5X,'VEL',3(F12.6,5X),'MACH',3(F20.6,5X),'MACH_R',F20.6,5x,'MACH',F20.6)     
     
    END 

    