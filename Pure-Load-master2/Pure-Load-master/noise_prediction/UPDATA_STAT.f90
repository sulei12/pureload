
    SUBROUTINE UPDATA_PANEL_VEL(IB,I_R,J_C,IK,TAO_PSI,BETA_RES,THETA_RES,VEL,VEL_N,VEL_N_DOT,MACH_I,MACH_I_DOT,LOAD,LOAD_DOT,LOAD_PRESSURE,LOAD_PRESSURE_DOT)
    USE ROTOR
    USE PARAMETEERS
    USE GLOBAL_PARAMETERS
    USE COOR_TRAN
    IMPLICIT NONE
    INTEGER,INTENT(IN)::IB,I_R,J_C,IK
    REAL(8),INTENT(IN)::TAO_PSI,BETA_RES,THETA_RES
    REAL(8),INTENT(OUT)::VEL(3),VEL_N,VEL_N_DOT,MACH_I(3),MACH_I_DOT(3),LOAD(3),LOAD_DOT(3),LOAD_PRESSURE,LOAD_PRESSURE_DOT

    REAL(8)::XPANEL(3),SPANEL,NPANEL(3),VPANEL(3),SPANEL_BAR
    REAL(8)::XPANEL_XYZ3(3),NPANEL_0(3),NPANEL_1(3),NPANEL_2(3),NPANEL_XYZ5(3),NPANEL_0_DOT(3)
    
    REAL(8)::TAO_PSI_DEG_1,TAO_PSI_DEG_2
    REAL(8)::BETA_RES_1,BETA_RES_2,THETA_RES_1,THETA_RES_2
    REAL(8)::LOAD1(3),LOAD2(3),COEF,LOAD_PRESSURE2,LOAD_PRESSURE1
    REAL(8)::VEL1(3),VEL2(3),VEL_N_1,VEL_N_2,MACH_I_1(3),MACH_I_2(3)
    
    REAL(8)::TAO_PSI_DEG,D_PSI,D_PSI_DEG
    
    
    
    
    !**角度的周期性
    D_PSI_DEG=5
    D_PSI=D_PSI_DEG*PI/180
    LOAD=0;LOAD_DOT=0;VEL=0;
    
    TAO_PSI_DEG=TAO_PSI*180/PI
    TAO_PSI_DEG=TAO_PSI_DEG-FLOOR((TAO_PSI_DEG-BLADE_SPACING(IB))/360)*360
    !角度转化到0-360区间
    
    ! IF (TAO_PSI_DEG .LE. (BLADE_SPACING(IB)+(IB-1)*360.0/N_BLADE))TAO_PSI_DEG=360.0+TAO_PSI_DEG
    
    
    TAO_PSI_DEG_1=TAO_PSI_DEG-D_PSI_DEG
    IF (TAO_PSI_DEG_1 .LE. PT_TIME(1,IB)) TAO_PSI_DEG_1=TAO_PSI_DEG_1+360.0
    IF (TAO_PSI_DEG_1 .GE. PT_TIME(NPT_TIME,IB))TAO_PSI_DEG_1=TAO_PSI_DEG_1-360.0
    
    TAO_PSI_DEG_2=TAO_PSI_DEG+D_PSI_DEG
    IF (TAO_PSI_DEG_2 .LE. PT_TIME(1,IB))TAO_PSI_DEG_2=TAO_PSI_DEG_2+360.0
    IF (TAO_PSI_DEG_2 .GE. PT_TIME(NPT_TIME,IB))TAO_PSI_DEG_2=TAO_PSI_DEG_2-360.0
    
    
    
    
    !**获取几何参数，桨叶坐标系
    IF(IK.EQ.1)THEN
        XPANEL=XPANEL_UP(:,I_R,J_C)
        SPANEL=SPANEL_UP(I_R,J_C)
        SPANEL_BAR=SPANEL/SUM(SPANEL_UP(I_R,:))
        NPANEL=NPANEL_UP(:,I_R,J_C)
    ELSE IF(IK.EQ.2)THEN
        XPANEL=XPANEL_DOWN(:,I_R,J_C)
        SPANEL=SPANEL_DOWN(I_R,J_C)
        SPANEL_BAR=SPANEL/SUM(SPANEL_DOWN(I_R,:))
        NPANEL=NPANEL_DOWN(:,I_R,J_C)
    END IF
    COEF=(XPANEL(2)+0.25*C_BLADE_R(I_R))/C_BLADE_R(I_R)
    
    
    
    load=0
    LOAD_DOT=0
    
    
     
    ! DIRECTION!!
    
    !**速度信息更新

    CALL TRAN_A35(AL_ROTOR,TAO_PSI,BETA_RES,THETA_RES,A35)
    XPANEL_XYZ3=MATMUL(A35,XPANEL)

    VPANEL(1)=XPANEL_XYZ3(2)*OMEGA
    VPANEL(2)=-XPANEL_XYZ3(1)*OMEGA
    VPANEL(3)=0
    
    CALL TRAN_A13(AL_ROTOR,TAO_PSI,BETA_RES,THETA_RES,A13)
    VEL=MATMUL(A13,VPANEL)
    VEL=V_INF+VEL       !+(/0,0,5/)
    
    CALL TRAN_A15(AL_ROTOR,TAO_PSI_DEG*PI/180,BETA_RES,THETA_RES,A15)
    NPANEL_0=MATMUL(A15,NPANEL)
    VEL_N=DOT_PRODUCT(NPANEL_0,VEL)
    MACH_I=VEL/SOUNDSPEED
    
    CALL LOADING_GET(TAO_PSI,IB,IK,I_R,J_C,LOAD,LOAD_DOT)
    CALL TRAN_A15(AL_ROTOR,TAO_PSI_DEG*PI/180,BETA_RES,THETA_RES,A15)
    !LOAD_PRESSURE=dot_product(load,NPANEL)   !for test

         
         
    LOAD=MATMUL(A15,LOAD)
    LOAD_PRESSURE=dot_product(load,NPANEL_0)
    !LOAD_PRESSURE=load(3)
    
    
    !____1______
    CALL UPDATE_STAT(IB,XPANEL(1),TAO_PSI_DEG_1*PI/180,BETA_RES_1,THETA_RES_1)
    CALL TRAN_A13(AL_ROTOR,TAO_PSI_DEG_1*PI/180,BETA_RES_1,THETA_RES_1,A13)    
    VEL1=MATMUL(A13,VPANEL)
    VEL1=V_INF+VEL1
    
    CALL TRAN_A15(AL_ROTOR,TAO_PSI_DEG_1*PI/180,BETA_RES_1,THETA_RES_1,A15)
    NPANEL_1=MATMUL(A15,NPANEL)
    VEL_N_1=DOT_PRODUCT(NPANEL_1,VEL1)
    MACH_I_1=VEL1/SOUNDSPEED
    
    CALL LOADING_GET(TAO_PSI_DEG_1*PI/180.0,IB,IK,I_R,J_C,LOAD1,LOAD_DOT)
    CALL TRAN_A15(AL_ROTOR,TAO_PSI_DEG_1*PI/180,BETA_RES_1,THETA_RES_1,A15)
    LOAD1=MATMUL(A15,LOAD1)
    LOAD_PRESSURE1=dot_product(load1,NPANEL_1)
    
    
    !____2______
    CALL UPDATE_STAT(IB,XPANEL(1),TAO_PSI_DEG_2*PI/180,BETA_RES_2,THETA_RES_2)
    CALL TRAN_A13(AL_ROTOR,TAO_PSI_DEG_2*PI/180,BETA_RES_2,THETA_RES_2,A13)
    VEL2=MATMUL(A13,VPANEL)
    VEL2=V_INF+VEL2
    
    CALL TRAN_A15(AL_ROTOR,TAO_PSI_DEG_2*PI/180,BETA_RES_2,THETA_RES_2,A15)
    NPANEL_2=MATMUL(A15,NPANEL)
    VEL_N_2=DOT_PRODUCT(NPANEL_2,VEL2)
    MACH_I_2=VEL2/SOUNDSPEED
    
    
    CALL LOADING_GET(TAO_PSI_DEG_2*PI/180.0,IB,IK,I_R,J_C,LOAD2,LOAD_DOT)
    CALL TRAN_A15(AL_ROTOR,TAO_PSI_DEG_2*PI/180,BETA_RES_2,THETA_RES_2,A15)  
    LOAD2=MATMUL(A15,LOAD2)
    LOAD_PRESSURE2=dot_product(load2,NPANEL_2)
    
    
    !_____DOT_____________
    VEL_N_DOT=(VEL_N_2-VEL_N_1)/(2*D_PSI/OMEGA)
    MACH_I_DOT=(MACH_I_2-MACH_I_1)/(2*D_PSI/OMEGA)
    
    LOAD_DOT=(LOAD2-LOAD1)/(2*D_PSI/OMEGA)
    !LOAD_PRESSURE_DOT=(LOAD_PRESSURE2-LOAD_PRESSURE1)/(2*D_PSI/OMEGA)
    
    NPANEL_0_DOT=(NPANEL_2-NPANEL_1)/(2*D_PSI/OMEGA)
    LOAD_PRESSURE_DOT=DOT_PRODUCT(LOAD,NPANEL_0_DOT)
    
    
    !WRITE(40,4100)tao_psi_deg,VEL_N_1,VEL_N_2,VEL_N_2-VEL_N_1,VEL_N_DOT,NPANEL_1,NPANEL_2,VEL_N
    !WRITE(40,4101)tao_psi_deg,VEL_N_1,VEL_N_2,VEL_N
4101 format(F12.6,5X,'VEL_N_1:',F12.6,5X,'VEL_N_2:',F12.6,5X,'VELN',F12.6,5x)
4100 FORMAT(F12.6,5X,'VEL_N_1:',F12.6,5X,'VEL_N_2:',F12.6,5X,'VELN-',F12.6,5x,'VELN',F12.6,5x,'npanel1',3(F12.6,5X),'npanel2',3(F12.6,5X),'xxx',f12.6)     
    !ACC=0
    
    END SUBROUTINE
    
        
    SUBROUTINE UPDATE_STAT(IB,X_R,TIME_PSI,BETA_RES,THETA_RES)
    ! 获取时间TAO时刻下桨叶各个截面部分的BETA和THETA
    USE PARAMETEERS
    USE ROTOR
    IMPLICIT NONE
    REAL(8),INTENT(IN):: X_R,TIME_PSI           !RAD
    INTEGER,INTENT(IN)::IB
    REAL(8),INTENT(OUT)::BETA_RES,THETA_RES     !RAD
    REAL(8) ::TIME_PSI_DEG
    
    TIME_PSI_DEG=TIME_PSI*180/PI
    TIME_PSI_DEG=TIME_PSI_DEG-FLOOR((TIME_PSI_DEG-BLADE_SPACING(IB))/360)*360
    
    !IF (TIME_PSI_DEG .LE. ((IB-1)*360.0/N_BLADE))   THEN
    !    TIME_PSI_DEG=360.0+TIME_PSI_DEG
    !END IF
    
    CALL LINEAR2D(NPT_R,NPT_TIME,PT_R,PT_TIME(:,IB),THETA_BLADE(:,:,IB),X_R,TIME_PSI_DEG,THETA_RES)
    CALL LINEAR2D(NPT_R,NPT_TIME,PT_R,PT_TIME(:,IB),BETA_BLADE(:,:,IB),X_R,TIME_PSI_DEG,BETA_RES)
    THETA_RES=THETA_RES*PI/180.0
    BETA_RES=BETA_RES*PI/180.0
    END SUBROUTINE 
    
    
    SUBROUTINE LOADING_DISTRIBUTION(ID_LOAD,IK,COEF,LOAD,C_BLADE)
    USE ROTOR
    USE PARAMETEERS
    USE GLOBAL_PARAMETERS
    IMPLICIT NONE
    INTEGER,INTENT(IN)::ID_LOAD,IK
    REAL(8),INTENT(IN)::COEF,C_BLADE
    REAL(8),INTENT(INOUT)::LOAD(3)
    

    IF(ID_LOAD .EQ. 1)THEN
    LOAD=2/PI*SQRT((1-COEF)/COEF)*LOAD/(C_BLADE*(R_BLADE_END-R_BLADE_BEGIN)/N_R)
    !load=load/(C_BLADE*(R_BLADE_END-R_BLADE_BEGIN)/N_R)
    !load=-load
    ! LOAD IS PRESSURE NOW~~ NOT FORCE
        IF(IK.EQ.1)THEN
            LOAD=LOAD*(-0.5)
        ELSEIF(IK.EQ.2)THEN
            LOAD=LOAD*0.5
        END IF
    ELSE 
    END IF
    END SUBROUTINE