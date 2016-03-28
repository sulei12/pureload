
    SUBROUTINE LOADING_GET(TAO_PSI,IB,IK,I_R,J_C,LOAD,LOAD_DOT)
    USE ROTOR
    USE PARAMETEERS
    USE GLOBAL_PARAMETERS
    USE COOR_TRAN
    
    use xml_data_noise_input_parameters
    use xml_data_noise_rotor_parameters
    use xml_data_noise_solver_parameters
    
    USE xml_data_compution_control
    
    IMPLICIT NONE
    !INTEGER,INTENT(IN) :: ID_LOAD 
    !ID_LOAD: 
    ! 1 FOR UH1H-EXP1 WOPWOP
    ! 3 FOR UH1H-EXP3 WOPWOP
    ! 5 FOR UH1H-EXP1 FREE-WAKE PRESSURE
    ! 7 FOR UH1H-EXP1 FREE-WAKE PRESSURE
    
    REAL(8),INTENT(IN):: TAO_PSI 
    INTEGER,INTENT(IN) :: IB,IK,I_R,J_C
    REAL(8),INTENT(OUT):: LOAD(3),LOAD_DOT(3)
    
    
    REAL(8)::TAO_PSI_DEG
    REAL(8)::BETA_RES,THETA_RES
    REAL(8)::XPANEL(3),SPANEL,NPANEL(3),VPANEL(3),SPANEL_BAR
    REAL(8)::COEF
    
    ! FOR CASE3 PARAMETERS
    REAL(8):: CT,VEL_XYZ3(3),VEL_XYZ5(3),UT,UP,UR
    REAL(8):: XPANEL_XYZ3(3),LOAD_XYZ5(3)
    REAL(8):: PHI,ALPHA,M2,VEL_TOL
    
    INTEGER::NXOC=19
    REAL(8)::V1,V2,XOC(19),V12(19),DVA12(19)
    REAL(8)::CLCORR,SQRTM2,CL,FALPHA,CPU,CPL,QDYN,SPU,SPL,CD0,CD1,SGMA,SIGMAU,SIGMAL
    
    CHARACTER(32)::COUM
    
    REAL(8):: ER,MACH,CD
    INTEGER:: I,J,K,L
    
    ! FOR CASE 5
    REAL(8):: DET_S,TEMP3(3)
    
    DATA XOC/0.0,0.5,1.25,2.5,5.0,7.5,10,15,20,25,30,40,50,60,70,80,90,95,100/
    DATA V12/0.0,0.8,1.005,1.114,1.174,1.184,1.188,1.188,1.183,1.174, 1.162,1.135,1.108,1.080,1.053,1.022,0.978,0.954,0.0/ 
    DATA DVA12/1.988,1.475,1.005,0.934,0.685,0.558,0.479,0.381,0.319,0.273,0.239,0.187,0.149,0.118,0.092,0.068,0.044,0.029,0.0/ 
    
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
   ! XPANEL=0.5*(XPANEL_UP(:,I_R,J_C)+XPANEL_DOWN(:,I_R,J_C))
    
    
        
        
        COEF=(XPANEL(2)+0.25*C_BLADE_R(I_R))/C_BLADE_R(I_R)
        CALL LINEAR1D(NXOC,XOC,V12,COEF*100,V1)
        CALL LINEAR1D(NXOC,XOC,DVA12,COEF*100,V2)
        
        TAO_PSI_DEG=TAO_PSI*180.0/PI
        TAO_PSI_DEG=TAO_PSI_DEG-FLOOR((TAO_PSI_DEG-blade_spacing(ib))/360)*360
        ER=(XPANEL(1)/R_BLADE_END)
        
        CL=0;CD=0;MACH=0;
        CALL LINEAR2D(NPT_R,NPT_TIME,LOADING_MATRIX(:,2,1,IB),PSI_ARRAY(:,IB),LOADING_MATRIX(:,4,:,IB),ER,TAO_PSI_DEG,CL)
        CALL LINEAR2D(NPT_R,NPT_TIME,LOADING_MATRIX(:,2,1,IB),PSI_ARRAY(:,IB),LOADING_MATRIX(:,5,:,IB),ER,TAO_PSI_DEG,CD)
        CALL LINEAR2D(NPT_R,NPT_TIME,LOADING_MATRIX(:,2,1,IB),PSI_ARRAY(:,IB),LOADING_MATRIX(:,3,:,IB),ER,TAO_PSI_DEG,MACH)
              
        
        CPU = 1.0-(V1+V2*CL)**2 
        CPL = 1.0-(V1-V2*CL)**2
        
        QDYN=0.5*RHO*(SOUNDSPEED*MACH)**2 
        SPU=QDYN*CPU 
        SPL=QDYN*CPL
        SIGMAU=0.5*QDYN*CD 
        SIGMAL=0.5*QDYN*CD 
        
        
        
        
        IF(IK.EQ.1)THEN
            LOAD(1)=0
            LOAD(2)= SIGMAU
            LOAD(3)= SPU
        ELSEIF(IK.EQ.2)THEN
            LOAD(1)=0
            LOAD(2)= SIGMAL
            LOAD(3)= SPL
        END IF
      
        
    
   
        ! FOR UH1H-EXP3 WOPWOP
        !CALL UPDATE_STAT(IB,XPANEL(1),TAO_PSI,BETA_RES,THETA_RES)
        !CALL TRAN_A35(AL_ROTOR,TAO_PSI,BETA_RES,THETA_RES,A35)
        !
        !CT = .0028 
        !UT = xpanel(1)/R_blade_end
        !UP = SQRT(CT/2) 
        !PHI = ATAN2(UP,UT) 
        !ALPHA = THETA_RES-PHI !aaE2
        !VEL_TOL=UT*OMEGA*R_BLADE_END
        !M2 = (VEL_TOL/SOUNDSPEED)**2 
        !
        !COEF=(XPANEL(2)+0.25*C_BLADE_R(I_R))/C_BLADE_R(I_R)
        !CALL LINEAR1D(NXOC,XOC,V12,COEF*100,V1)
        !CALL LINEAR1D(NXOC,XOC,DVA12,COEF*100,V2)
        !
        !CLCORR = 1. 
        !IF((XPANEL(1)/R_BLADE_END).GT.0.9)CLCORR=(10.0*(1-(XPANEL(1)/R_BLADE_END)))**2 
        !SQRTM2= 1.0/SQRT(1.0-M2) 
        !CL = 5.7*ALPHA*SQRTM2 
        !FALPHA = CL*CLCORR 
        !CPU = 1.0-(V1+V2*FALPHA)**2 
        !CPL = 1.0-(V1-V2*FALPHA)**2 
        !
        !QDYN = 0.5*RHO*VEL_TOL*VEL_TOL
        !SPU = QDYN*CPU 
        !SPL = QDYN*CPL
        !!DP = SPL - SPU 
        !
        !CD0 = 0.006 
        !CD1 = 0.004
        !SGMA = (CD0+CD1*CL*CL)*QDYN 
        !SIGMAU= SGMA* 0.5 
        !SIGMAL= SGMA* 0.5 
        !
        !IF(IK.EQ.1)THEN
        !    LOAD(1)=0
        !    LOAD(2)= SIGMAU
        !    LOAD(3)= SPU
        !ELSEIF(IK.EQ.2)THEN
        !    LOAD(1)=0
        !    LOAD(2)= SIGMAL
        !    LOAD(3)= SPL
        !END IF
        !
        !
        !
        
    
        !WRITE(*,*) 'ERROR IN THE SUBROUTINE LOADING_GET'
  
    
    
        LOAD_DOT=0
        LOAD_XYZ5(2)=LOAD(2)*NPANEL(3)+LOAD(3)*NPANEL(2)
        LOAD_XYZ5(3)=LOAD(3)*NPANEL(3)-LOAD(2)*NPANEL(2)
        LOAD_XYZ5(1)=0
        LOAD=LOAD_XYZ5
        

           
    
    END SUBROUTINE