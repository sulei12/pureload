
    
    SUBROUTINE INITIAL_DATA()
    IMPLICIT NONE
    CALL OPENFILE()
    CALL INIT_MEMORY()
    CALL INITIAL_GLOBAL_INFO()
    CALL INITIAL_BLADE_INFO()
    CALL INITIAL_BLADE_FINITE_ELEMENT()
    CALL OUTPUT_CHECK()
    write(*,*)'INITIAL FINISHED'
    END SUBROUTINE
    
    SUBROUTINE INITIAL_GLOBAL_INFO()
    USE GLOBAL_PARAMETERS
    USE PARAMETEERS
    USE COOR_TRAN
    USE ROTOR,ONLY:AL_ROTOR
    !USE xml_data_noise_parameters
    use xml_data_noise_input_parameters
    use xml_data_noise_rotor_parameters
    use xml_data_noise_solver_parameters
    
    
    IMPLICIT NONE
    REAL(8)::X_VISIT_NEW(3)
    N_R=20;
    N_C=20;
    N_R=N_R_INPUT
    N_C=N_C_INPUT
    
    ID_LOADING=1
    !ID_LOADING=1，非紧致弦分布
    !ID_LOADING=2，？？
    
    NTIME=NTIME_INPUT
    PSI_MAX_DEG=360
    DET_PSI_DEG=PSI_MAX_DEG/NTIME
    DET_PSI=DET_PSI_DEG*PI/180
    
    X_VISIT=X_VISIT_INPUT
    V_INF=V_INF_INPUT
    AL_ROTOR=ROTOR1%AL_ROTOR_INPUT*PI/180
    
    END SUBROUTINE INITIAL_GLOBAL_INFO
    
    
    
    SUBROUTINE INITIAL_BLADE_INFO()
    USE PARAMETEERS
    USE GLOBAL_PARAMETERS
    USE ROTOR
    !use xml_data_noise_parameters
    use xml_data_noise_input_parameters
    use xml_data_noise_rotor_parameters
    use xml_data_noise_solver_parameters
    
    
    IMPLICIT NONE
    INTEGER :: ID_AERO
    INTEGER :: ID_CBLADE=1
    INTEGER :: I_R
    REAL(8)::C_BLADE,X_R
    
    INTEGER :: I_N,IB
    
    

    N_BLADE=ROTOR1%NBLADE_INPUT
    ALLOCATE(BLADE_SPACING(N_BLADE))
    !BLADE_SPACING=0
    DO IB=1,N_BLADE
        !BLADE_SPACING(IB)=360/N_BLADE*(IB-1)
        BLADE_SPACING(IB)=ROTOR1%BLADE_SPACING_input(IB)
    END DO
    
    !BLADE_SPACING(1)=0
    !BLADE_SPACING(2)=60
    !BLADE_SPACING(3)=180
    !BLADE_SPACING(4)=240
    !UNIFORM SPACING
    !R_BLADE_BEGIN=0.155
    !R_BLADE_END=1.829
    
    R_BLADE_BEGIN=ROTOR1%R_BLADE_BEGIN_INPUT
    R_BLADE_END=ROTOR1%R_BLADE_END_INPUT
    
    ALLOCATE(C_BLADE_R(N_R))
    IF( (ROTOR1%R_CONTROL_INPUT(1)*R_BLADE_END).LT. R_BLADE_BEGIN )     ROTOR1%R_CONTROL_INPUT(1)=R_BLADE_BEGIN/R_BLADE_END
    DO I_R=1,N_R
        DO I_N=1,ROTOR1%N_CONTROL_INPUT-1
            X_R=R_BLADE_BEGIN+(R_BLADE_END-R_BLADE_BEGIN)/N_R*(I_R-0.5)
            IF( (X_R-ROTOR1%R_CONTROL_INPUT(I_N)*R_BLADE_END)*(X_R-ROTOR1%R_CONTROL_INPUT(I_N+1)*R_BLADE_END).LE.0.0 )THEN
                C_BLADE_R(I_R) = ROTOR1%C_BLADE_INPUT(I_N+1)-(X_R-ROTOR1%R_CONTROL_INPUT(I_N+1)*R_BLADE_END)*(ROTOR1%C_BLADE_INPUT(I_N+1)-ROTOR1%C_BLADE_INPUT(I_N))/(ROTOR1%R_CONTROL_INPUT(I_N+1)-ROTOR1%R_CONTROL_INPUT(I_N))/R_BLADE_END
            END IF
        END DO
    END DO

    
    
    
    !AL_ROTOR=8.85*PI/180
    !AL_ROTOR=0*PI/180
    OMEGA=ROTOR1%OMEGA_INPUT
    
    !N_BLADE=2
    !R_BLADE_BEGIN=0.2
    !R_BLADE_END=1.0
    !C_BLADE=0.1
    !AL_ROTOR=0
    !OMEGA=200
    
    IF (PSI_MAX_DEG .LT. (T_VISIT*OMEGA*180/PI)) THEN
        WRITE(*,*)'T_VISIT EXPANDS'
        STOP
    ENDIF
    
        
    
    ID_AERO=2
    IF (ID_AERO.EQ. 1)THEN
        !CALL AERODY()
    ELSEIF(ID_AERO.EQ.2)THEN
        CALL READ_AIRFOIL
        CALL READ_CONTROL_POINT
        
        
        CALL READ_BLADE_THETA
        CALL READ_BLADE_BETA
        
        CALL READ_BLADE_LOAD
        CALL LOADING_TRAN()
        
        CALL INIT_SPEC
        !CALL READ_VINDUCE
    ELSE
        WRITE(*,*)'ERROR'
    ENDIF
    !BETA_BLADE=0
    !THETA_BLADE=0
  
    END SUBROUTINE INITIAL_BLADE_INFO
    
    
    
    SUBROUTINE READ_CONTROL_POINT()
    USE ROTOR
    USE PARAMETEERS
    USE GLOBAL_PARAMETERS,ONLY:NTIME
    USE xml_data_compution_control
    IMPLICIT NONE
    INTEGER:: I,J,ib
    CHARACTER*60 COUM
    OPEN(100,FILE=TRIM(OUTPUT_FOLDERNAME)//'\BLADE_CONTROL_POINT.INP')
    READ(100,*) COUM
    READ(100,*) COUM
    READ(100,*) NPT_R
    READ(100,*) COUM   
    NPT_TIME=NTIME+1
    ALLOCATE(PT_R(NPT_R))
    ALLOCATE(PT_TIME(NPT_TIME,N_BLADE))
    READ(100,*)(PT_R(I),I=1,NPT_R)
    READ(100,*) COUM
    
    
       DO IB=1,N_BLADE
        DO I=1,NPT_TIME
            PT_TIME(I,IB)=(I-1)*360.0/NTIME+BLADE_SPACING(IB)
        END DO
       END DO
       
    
    
    END SUBROUTINE
    
    
    SUBROUTINE READ_BLADE_BETA()
    USE ROTOR
    USE PARAMETEERS
    USE xml_data_compution_control
    USE xml_data_noise_input_parameters
    USE xml_data_noise_rotor_parameters
    USE xml_data_noise_solver_parameters
    
    IMPLICIT NONE
    INTEGER:: I,J,IB
    CHARACTER*60 COUM
    

    ALLOCATE(BETA_BLADE(NPT_R,NPT_TIME,N_BLADE))
    ALLOCATE(DBETA_BLADE(NPT_R,NPT_TIME,N_BLADE))
    ALLOCATE(D2BETA_BLADE(NPT_R,NPT_TIME,N_BLADE))
    
    !OPEN(100,FILE=TRIM(OUTPUT_FOLDERNAME)//'\blade_BETA.INP')
    !READ(100,*) COUM
    !DO IB=1,N_BLADE
    !    READ(100,*)COUM
    !    READ(100,*)COUM
    !    DO I=1,NPT_TIME-1
    !        READ(100,*) COUM,PT_TIME(I,IB)
    !        READ(100,*) (BETA_BLADE(J,I,IB),J=1,NPT_R)
    !        READ(100,*) COUM
    !        READ(100,*) (DBETA_BLADE(J,I,IB),J=1,NPT_R)
    !        READ(100,*) COUM
    !        READ(100,*) (D2BETA_BLADE(J,I,IB),J=1,NPT_R)
    !    END DO
    !END DO
    !!PT_TIME(NPT_TIME,IB)=2*PT_TIME(NPT_TIME-1,IB)-PT_TIME(NPT_TIME-2,IB)
    !READ(100,*) COUM
    !CLOSE(100)
    
     DO IB=1,N_BLADE
        DO I=1,NPT_TIME-1
            do j=1,npt_r
            BETA_BLADE(J,I,IB)=ROTOR1%BETA_INPUT(1)-ROTOR1%BETA_INPUT(2)*COS(PT_TIME(I,IB)*PI/180)-ROTOR1%BETA_INPUT(3)*SIN(PT_TIME(I,IB)*PI/180)
            DBETA_BLADE(J,I,IB)=0
            D2BETA_BLADE(J,I,IB)=0
            end do
        END DO
     END DO
     
    BETA_BLADE(:,NPT_TIME,:)=BETA_BLADE(:,1,:)
    DBETA_BLADE(:,NPT_TIME,:)=DBETA_BLADE(:,1,:)
    D2BETA_BLADE(:,NPT_TIME,:)=D2BETA_BLADE(:,1,:)
    

    
    !BETA_BLADE=BETA_BLADE*PI/180
    !DBETA_BLADE=DBETA_BLADE*PI/180
    !D2BETA_BLADE=D2BETA_BLADE*PI/180
    
    END SUBROUTINE 
    
    
    SUBROUTINE READ_BLADE_THETA()
    USE ROTOR
    USE xml_data_compution_control
    USE PARAMETEERS
    USE xml_data_noise_input_parameters
    USE xml_data_noise_rotor_parameters
    USE xml_data_noise_solver_parameters
    
    IMPLICIT NONE
    INTEGER:: I,J,IB
    CHARACTER*60 COUM
    
    ALLOCATE(THETA_BLADE(NPT_R,NPT_TIME,N_BLADE))
    
    !OPEN(100,FILE=TRIM(OUTPUT_FOLDERNAME)//'\blade_attack.INP')
    !READ(100,*) COUM
    !DO IB=1,N_BLADE
    !    READ(100,*)COUM
    !    READ(100,*)COUM
    !    DO I=1,NPT_TIME-1
    !        READ(100,*) COUM,PT_TIME(I,IB)
    !        READ(100,*) (THETA_BLADE(J,I,IB),J=1,NPT_R)
    !    END DO
    !END DO
    !READ(100,*) COUM
    !CLOSE(100)

    !THETA_BLADE=THETA_BLADE*3.14/180
    !THETA_BLADE(:,NPT_TIME,:)=THETA_BLADE(:,1,:)
 
    
    
    DO IB=1,N_BLADE
        DO I=1,NPT_TIME-1
            do j=1,npt_r
            THETA_BLADE(j,i,IB)=ROTOR1%THET_INPUT(1)-ROTOR1%THET_INPUT(2)*COS(PT_TIME(I,IB)*PI/180)-ROTOR1%THET_INPUT(3)*SIN(PT_TIME(I,IB)*PI/180)+ROTOR1%THETT*(2*j-1)/(2*npt_r)
            !THETA_BLADE(j,:,IB)=15
            end do
        END DO 
    END DO
    !write(*,*)ROTOR1%THETT
    !write(*,*)ROTOR1%BETA_INPUT(1),ROTOR1%BETA_INPUT(2),ROTOR1%BETA_INPUT(3)
    !THETA_BLADE=THETA_BLADE*3.14/180
    THETA_BLADE(:,NPT_TIME,:)=THETA_BLADE(:,1,:)
    END SUBROUTINE 
    
        
    SUBROUTINE READ_BLADE_LOAD()
    USE ROTOR
    USE GLOBAL_PARAMETERS
    USE xml_data_compution_control
    !xx
    IMPLICIT NONE
    INTEGER:: I,J,K,IB
    CHARACTER*60 COUM
    OPEN(100,FILE=TRIM(OUTPUT_FOLDERNAME)//'\blade_LOADING.INP')
    READ(100,*) COUM
    ALLOCATE(FORCE_CELL(3,NPT_R,NPT_TIME,N_BLADE))
    DO IB=1,N_BLADE
        READ(100,*)COUM
        READ(100,*)COUM
        DO I=1,NPT_TIME-1
            READ(100,*) COUM,PT_TIME(I,IB)
            DO J=1,NPT_R
                READ(100,*) (FORCE_CELL(K,J,I,IB),K=1,3)
            END DO
        END DO
    END DO
    READ(100,*) COUM
    CLOSE(100)
    
    FORCE_CELL(:,:,NPT_TIME,:)=FORCE_CELL(:,:,1,:)
    ! 这里读入的载荷load是在坐标系XYZ5中。注意freewake程序输出结果的坐标系定义与本程序中定义不同。
    
    !

    END SUBROUTINE
    
    SUBROUTINE LOADING_TRAN()
    ! TRANSLATE TO LOADING_GET.INP
    
    USE ROTOR
    USE PARAMETEERS
    USE GLOBAL_PARAMETERS
    USE COOR_TRAN
    use xml_data_noise_input_parameters
    use xml_data_noise_rotor_parameters
    use xml_data_noise_solver_parameters
    USE xml_data_compution_control
    
    
    IMPLICIT NONE
    REAL(8)::TEMP3(3),MACH,VEL_TOL,DET_S,QDYN,CL,CD,ER
    INTEGER::I,J,K,IB
    character*80::coum
    
! WAITING FOR MODIFING

        OPEN(110,FILE=TRIM(OUTPUT_FOLDERNAME)//'\loading_input.INP')
        WRITE(110,*)NPT_R,NPT_TIME
        WRITE(110,*)
        
        DO IB=1,N_BLADE
            WRITE(110,*)'IB=',IB
            DO I=1,NPT_TIME-1
                WRITE(110,*)'PSI=',PT_TIME(I,IB)
                DO J=NPT_R,1,-1
                    ER=PT_R(J)/R_BLADE_END
        
                    CALL TRAN_A31(AL_ROTOR,PT_TIME(I,IB)*PI/180.0,A31)
                    TEMP3=MATMUL(A31,V_INF)
                    VEL_TOL=PT_R(J)*OMEGA-TEMP3(2)
                    MACH=VEL_TOL/SOUNDSPEED
                    !DET_S=(R_BLADE_END-R_BLADE_BEGIN)*C_BLADE_R(1)/N_R
                    DET_S=(R_BLADE_END-0)*C_BLADE_R(1)/N_R
                    QDYN=0.5*RHO*VEL_TOL**2  
                    CL=FORCE_CELL(3,J,I,IB)/QDYN/DET_S
                    CD=-FORCE_CELL(2,J,I,IB)/QDYN/DET_S
                    if (er .le. 0.20)then
                        cl=0
                        cd=0
                    end if
                
                    WRITE(110,1000)J,ER,MACH,CL,CD
                
                END DO
                WRITE(110,*)
            END DO
        END DO
        
    CLOSE(110)
    
    ! 用均匀入流的简单气动模型
    ! CALL GET_DATA_WOW3()
     
    
    OPEN(100,FILE=TRIM(OUTPUT_FOLDERNAME)//'\loading_input.INP')
    READ(100,*) NPT_R,NPT_TIME
    ALLOCATE(LOADING_MATRIX(NPT_R,5,NPT_TIME,N_BLADE))
    ALLOCATE(PSI_ARRAY(NPT_TIME,N_BLADE))
    DO IB=1,N_BLADE
        READ(100,*)COUM
        DO I=1,NPT_TIME-1
            READ(100,*)COUM
            PSI_ARRAY(I,IB)=360.0/(NPT_TIME-1)*(I-1)+BLADE_SPACING(IB)
            DO J=1,NPT_R
                READ(100,*) (LOADING_MATRIX(J,K,I,IB),K=1,5)
                
                !ONLY FOR TEST
                IF(IB.EQ.2)THEN
                    IF(I.LE.18)THEN
                        LOADING_MATRIX(J,:,I,2)=LOADING_MATRIX(J,:,I+18,1)
                    ELSE IF(I.GE.19)THEN
                        LOADING_MATRIX(J,:,I,2)=LOADING_MATRIX(J,:,I-18,1)
                    END IF
                END IF
                
            END DO
        END DO
    END DO
    
    LOADING_MATRIX(:,:,NPT_TIME,:)=LOADING_MATRIX(:,:,1,:)
    PSI_ARRAY(NPT_TIME,:)=PSI_ARRAY(1,:)+360
    CLOSE(100)
    INIT_LOADING=0
    
    
1000 FORMAT(I3,3X,F5.2,3X,F8.3,3X,F8.4,3X,F8.5)
    END SUBROUTINE
    
    
    
    SUBROUTINE GET_DATA_WOW3()
    
    !! ONLY FOR TEST 
    !! UNIFORM FLOW 
    
    USE ROTOR
    USE PARAMETEERS
    USE GLOBAL_PARAMETERS
    USE COOR_TRAN
    use xml_data_noise_input_parameters
    use xml_data_noise_rotor_parameters
    use xml_data_noise_solver_parameters
    USE xml_data_compution_control
    IMPLICIT NONE
    INTEGER::I,J,K,IB
    REAL(8)::CT,UT,UP,PHI,CL,CD,CD0,CD1,CLCORR,SQRTM2,QDYN,ER,M2,MACH,ALPHA,VEL_TOL,BETA_RES,THETA_RES
    
    OPEN(100,FILE=TRIM(OUTPUT_FOLDERNAME)//'\loading_input.INP')
        WRITE(100,*) NPT_R,NPT_TIME
        WRITE(100,*)
        DO IB=1,N_BLADE
            WRITE(100,*)'IB=',IB
            DO I=1,NPT_TIME-1
            WRITE(100,*)'PSI=',360.0/(NPT_TIME-1)*(I-1)+BLADE_SPACING(IB)
                DO J=NPT_R,1,-1
                    CALL UPDATE_STAT(1,PT_R(J),PT_TIME(I,1)*PI/180+1E-5,BETA_RES,THETA_RES)
                    CALL TRAN_A35(AL_ROTOR,PT_TIME(I,1)*PI/180+1E-5,BETA_RES,THETA_RES,A35)
                    CT = .0028 
                    UT = PT_R(J)/R_blade_end
                    UP = SQRT(CT/2)
                    PHI = ATAN2(UP,UT) 
                    ALPHA = THETA_RES-PHI !aaE2
                    VEL_TOL=UT*OMEGA*R_BLADE_END
                    MACH=(VEL_TOL/SOUNDSPEED)
                    M2 = MACH**2 
                    ER=UT
                    CLCORR = 1. 
                    IF((PT_R(J)/R_BLADE_END).GT.0.9)    CLCORR=(10.0*(1-(PT_R(J)/R_BLADE_END)))**2 
                    SQRTM2= 1.0/SQRT(1.0-M2) 
                    CL = 5.7*ALPHA*SQRTM2*CLCORR 
                    QDYN = 0.5*RHO*VEL_TOL*VEL_TOL
                    CD0 = 0.006 
                    CD1 = 0.004
                    CD = (CD0+CD1*CL*CL)
                    WRITE(100,1000)J,ER,MACH,CL,CD   
                END DO
                WRITE(100,*)
            END DO
        END DO
        CLOSE(100)
        
1000    FORMAT(I3,3X,F5.2,3X,F8.3,3X,F8.4,3X,F8.5)
        
    END SUBROUTINE
    
    
    SUBROUTINE READ_VINDUCE()
    USE ROTOR
    USE GLOBAL_PARAMETERS
    USE xml_data_compution_control
    IMPLICIT NONE
    INTEGER:: I,J,K,IB
    REAL(8)::CT
    CHARACTER*60 COUM
    ALLOCATE(V_INDUCE(3,NPT_R,NPT_TIME,N_BLADE))
    OPEN(100,FILE=TRIM(OUTPUT_FOLDERNAME)//'\blade_INDUCE.INP')
    READ(100,*) COUM
    DO IB=1,N_BLADE
        READ(100,*)COUM
        READ(100,*)COUM
        DO I=1,NPT_TIME-1
            READ(100,*) COUM,PT_TIME(I,IB)
            DO J=1,NPT_R
                READ(100,*) V_INDUCE(3,J,I,IB)
            END DO
        END DO
    END DO
    READ(100,*) COUM
    !FORCE_CELL=FORCE_CELL/2
    CLOSE(100)
    
    V_INDUCE(:,:,NPT_TIME,:)=V_INDUCE(:,:,1,:)
    V_INDUCE(:,:,:,:)=0
    
    PT_TIME(NPT_TIME,:)=2*PT_TIME(NPT_TIME-1,:)-PT_TIME(NPT_TIME-2,:)
    END SUBROUTINE
    
    
    
    
    SUBROUTINE INITIAL_BLADE_FINITE_ELEMENT()
    USE GLOBAL_PARAMETERS
    USE ROTOR
    USE AIRFOIL
    use xml_data_noise_input_parameters
    use xml_data_noise_rotor_parameters
    use xml_data_noise_solver_parameters
    
    IMPLICIT NONE
    !供备份。未使用
    !计算面元中点位置
    !随体坐标系
    
 
    INTEGER :: I,J,K
    REAL(8) :: X_PL,Y_PL,Z_PL  !!在随叶坐标系中IN X5Y5Z5
    REAL(8) :: LTH_PANEL_R,LTH_PANEL_C

    REAL(8) :: Q_VEC(3),XUNIT(3)
    REAL(8) :: MAX=1.0E31
    REAL(8) :: IB
    REAL(8) :: Z1(3),Z2(3)
    REAL(8),ALLOCATABLE::R_POSITION(:),C_POSITION(:),R_LTH(:),C_LTH(:)
    REAL(8)::C1,C2,fyy,fyy_1
    INTEGER::N1,N2
    !
    ALLOCATE(XPANEL_UP(3,N_R,N_C))
    ALLOCATE(SPANEL_UP(N_R,N_C))
    ALLOCATE(NPANEL_UP(3,N_R,N_C))
    ALLOCATE(XPANEL_DOWN(3,N_R,N_C))
    ALLOCATE(SPANEL_DOWN(N_R,N_C))
    ALLOCATE(NPANEL_DOWN(3,N_R,N_C))
    ALLOCATE(R_POSITION(N_R))
    ALLOCATE(C_POSITION(N_C))
    ALLOCATE(R_LTH(N_R))
    ALLOCATE(C_LTH(N_C))
    
    
    XUNIT=(/1,0,0/)
    
    
    ! 判断是否为等分单元

    if(r_para1 .eq.0)  r_para1=1.0/N_R
    if(r_para2 .eq.0)  r_para2=1.0/N_R
    
    
    IF (EQRGRID.EQ.1)THEN
        LTH_PANEL_R=(R_BLADE_END-R_BLADE_BEGIN)/N_R
        DO I=1,N_R
            R_LTH(I)=LTH_PANEL_R   
            R_POSITION(I)=R_BLADE_BEGIN+LTH_PANEL_R*(I-0.5)
        END DO
    ELSEIF(EQRGRID.EQ.0)THEN
        WRITE(*,*) 'NOT EQ: R'
            C1=1.1;C2=1.1
        fyy=-10;fyy_1=-10
        N1=1
        DO I=1,n_r
            N2=floor(1+(log10(r_para1*(R_BLADE_END-R_BLADE_BEGIN))-log10(r_para2*(R_BLADE_END-R_BLADE_BEGIN))+(n1-1)*log10(c1))/log10(c2))
            if(c2.eq.1)n2=0
            fyy=r_para1*(R_BLADE_END-R_BLADE_BEGIN)*(c1**n1-1)/(c1-1)+r_para2*(R_BLADE_END-R_BLADE_BEGIN)*(c2**n2-1)/(c2-1)+r_para1*(R_BLADE_END-R_BLADE_BEGIN)*c1**(n1-1)*(n_r-n1-n2)-(R_BLADE_END-R_BLADE_BEGIN)
            
            if(fyy*fYY_1 .lt. 0)then 
                exit
            end if
            fyy_1=fyy
            if(fyy .gt.0 )n1=n1-1
            if(fyy .lt.0 )n1=n1+1
        END DO
        
        do i=1,n_r
            if (i.le. n1)then
                r_lth(i)=r_para1*(R_BLADE_END-R_BLADE_BEGIN)*c1**(i-1)
            else if(i.ge.(n_r-n2+1))then
                r_lth(i)=r_para2*(R_BLADE_END-R_BLADE_BEGIN)*c2**(n_r-i)
            else
                r_lth(i)=r_para1*(R_BLADE_END-R_BLADE_BEGIN)*c1**(n1-1)
            end if
        end do
        r_lth=r_lth/sum(r_lth)*(R_BLADE_END-R_BLADE_BEGIN)
        do i=1,n_r
            r_position(i)=sum(r_lth(1:i))-r_lth(i)/2+R_BLADE_BEGIN
        end do
        write(*,*)sum(r_lth)
    END IF
    
    
    
    
    !等分单元
    
    
    
    
    
    !
    DO IB=1,N_BLADE
        DO K=1,2
            DO I=1,N_R
                
                IF (EQCGRID.EQ.1)THEN
                    LTH_PANEL_C=C_BLADE_R(I)/N_C
                    DO J=1,N_C      
                        C_LTH(J)=LTH_PANEL_C
                        C_POSITION(J)=LTH_PANEL_C*(J-0.5)
                    END DO
                ELSEIF(EQcGRID.EQ.0)THEN
                    if(c_para1 .eq.0)  C_para1=1.0/N_C
                    if(c_para2 .eq.0)  c_para2=1.0/N_C
                    WRITE(*,*) 'NOT EQ: C'
                    C1=1.15;C2=C1
                    n1=0;n2=0
                        fyy=-10;fyy_1=-10
                        N1=1
                        DO j=1,n_c
                            N2=floor(1+(log10(c_PARA1*c_blade_r(i))-log10(c_PARA2*c_blade_r(i))+(n1-1)*log10(c1))/log10(c2))
                            fyy=c_PARA1*c_blade_r(i)*(c1**n1-1)/(c1-1)+c_PARA2*c_blade_r(i)*(c2**n2-1)/(c2-1)+c_PARA1*c_blade_r(i)*c1**(n1-1)*(n_c-n1-n2)-(C_BLADE_R(i))
            
                            if(fyy*fYY_1 .lt. 0)then 
                                exit
                            end if
                            fyy_1=fyy
                            if(fyy .gt.0 )n1=n1-1
                            if(fyy .lt.0 )n1=n1+1
                        END DO
        
                        do j=1,n_c
                            if (j.le. n1)then
                                c_lth(j)=c_PARA1*c_blade_r(i)*c1**(j-1)
                            else if(j.ge.(n_c-n2+1))then
                                c_lth(j)=c_PARA2*c_blade_r(i)*c2**(n_c-j)
                            else
                                c_lth(j)=c_PARA1*c_blade_r(i)*c1**(n1-1)
                            end if
                        end do
                        c_lth=c_lth/sum(c_lth)*C_BLADE_R(i)
                        do j=1,n_c
                            c_position(j)=sum(c_lth(1:j))-c_lth(j)/2
                        end do
                        write(*,*)sum(c_lth)
                        write(*,*)'n1,n2:',n1,n2
                END IF
                
                    
                    
                !LTH_PANEL_C=C_BLADE_R(I)/N_C
                AIRFOIL_POINT_UP=AIRFOIL_POINT_UP*C_BLADE_R(I)
                AIRFOIL_POINT_DOWN=AIRFOIL_POINT_DOWN*C_BLADE_R(I)
                
                DO J=1,N_C 
                    X_PL=R_POSITION(I)
                    Y_PL=C_POSITION(J)   !原点取在翼型前缘点     
                   ! CALL spline(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),NAIRFOIL,MAX,MAX,Y2A)
                   ! CALL splint(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),Y2A,NAIRFOIL,Y_PL,Z_PL)
            
                    IF (K.EQ.1) THEN
                        CALL spline(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),NAIRFOIL,MAX,MAX,Y2A)
                        CALL splint(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),Y2A,NAIRFOIL,Y_PL,Z_PL)
                        Y_PL=Y_PL-0.25*C_BLADE_R(I)   
                        XPANEL_UP(:,I,J)=(/X_PL,Y_PL,-Z_PL/)
                        !SPANEL_UP(I,J)=LTH_PANEL_R*LTH_PANEL_C
                        
                        Z1(1)=0
                        !Z1(2)=Y_PL-C_BLADE_R(I)/(2*N_C)+0.25*C_BLADE_R(I)
                        Z1(2)=Y_PL-C_LTH(J)/2+0.25*C_BLADE_R(I)
                        CALL spline(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),NAIRFOIL,MAX,MAX,Y2A)
                        CALL splint(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),Y2A,NAIRFOIL,Z1(2),Z1(3))
                        Z2(1)=0
                        Z2(2)=Y_PL+C_LTH(J)/2+0.25*C_BLADE_R(I)
                        CALL spline(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),NAIRFOIL,MAX,MAX,Y2A)
                        CALL splint(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),Y2A,NAIRFOIL,Z2(2),Z2(3))
                        
                        SPANEL_UP(I,J)=R_LTH(I)*SQRT(SUM((Z2-Z1)*(Z2-Z1)))
                        !SPANEL_UP(I,J)=LTH_PANEL_R*SQRT(DOT_PRODUCT((Z2-Z1),(Z2-Z1)))
                        !WRITE(*,*)
                    ELSE 
                        CALL spline(AIRFOIL_POINT_DOWN(:,1),AIRFOIL_POINT_DOWN(:,2),NAIRFOIL,MAX,MAX,Y2A)
                        CALL splint(AIRFOIL_POINT_DOWN(:,1),AIRFOIL_POINT_DOWN(:,2),Y2A,NAIRFOIL,Y_PL,Z_PL)
                        Y_PL=Y_PL-0.25*C_BLADE_R(I)   
                        XPANEL_DOWN(:,I,J)=(/X_PL,Y_PL,-Z_PL/)
                        !SPANEL_DOWN(I,J)=LTH_PANEL_R*LTH_PANEL_C
                        
                        Z1(1)=0
                        Z1(2)=Y_PL-C_LTH(J)/2+0.25*C_BLADE_R(I)
                        CALL spline(AIRFOIL_POINT_DOWN(:,1),AIRFOIL_POINT_DOWN(:,2),NAIRFOIL,MAX,MAX,Y2A)
                        CALL splint(AIRFOIL_POINT_DOWN(:,1),AIRFOIL_POINT_DOWN(:,2),Y2A,NAIRFOIL,Z1(2),Z1(3))
                        Z2(1)=0
                        Z2(2)=Y_PL+C_LTH(J)/2+0.25*C_BLADE_R(I)
                        CALL spline(AIRFOIL_POINT_DOWN(:,1),AIRFOIL_POINT_DOWN(:,2),NAIRFOIL,MAX,MAX,Y2A)
                        CALL splint(AIRFOIL_POINT_DOWN(:,1),AIRFOIL_POINT_DOWN(:,2),Y2A,NAIRFOIL,Z2(2),Z2(3))
                        
                        SPANEL_DOWN(I,J)=R_LTH(I)*SQRT(SUM((Z2-Z1)*(Z2-Z1)))
                        
                    ENDIF
                END DO
                
                AIRFOIL_POINT_UP=AIRFOIL_POINT_UP/C_BLADE_R(I)
                AIRFOIL_POINT_DOWN=AIRFOIL_POINT_DOWN/C_BLADE_R(I)
                
            END DO
        END DO
    END DO
    
    
    
    !
    DO K=1,2
    DO I=1,N_R
        DO J=1,N_C
            IF (K.EQ.1) THEN      
                IF (J.EQ.1) THEN
                    Q_VEC=(/DBLE(0),XPANEL_UP(2,I,J+1)-XPANEL_UP(2,I,J),XPANEL_UP(3,I,J+1)-XPANEL_UP(3,I,J)/)
                ELSE
                    Q_VEC=(/DBLE(0),XPANEL_UP(2,I,J)-XPANEL_UP(2,I,J-1),XPANEL_UP(3,I,J)-XPANEL_UP(3,I,J-1)/)
                END IF
                CALL VECTOR_CROSS3(NPANEL_UP(:,I,J),-XUNIT,Q_VEC)
                CALL UNIT3(NPANEL_UP(:,I,J))            
            ELSE
                IF (J.EQ.1) THEN
                    Q_VEC=(/DBLE(0),XPANEL_DOWN(2,I,J+1)-XPANEL_DOWN(2,I,J),XPANEL_DOWN(3,I,J+1)-XPANEL_DOWN(3,I,J)/)
                ELSE
                    Q_VEC=(/DBLE(0),XPANEL_DOWN(2,I,J)-XPANEL_DOWN(2,I,J-1),XPANEL_DOWN(3,I,J)-XPANEL_DOWN(3,I,J-1)/)
                END IF
                CALL VECTOR_CROSS3(NPANEL_DOWN(:,I,J),XUNIT,Q_VEC)
                CALL UNIT3(NPANEL_DOWN(:,I,J))            
            END IF
        END DO
    END DO
    END DO
    
    
    DEALLOCATE(R_POSITION)
    DEALLOCATE(C_POSITION)
    DEALLOCATE(R_LTH)
    DEALLOCATE(C_LTH)
    
    END SUBROUTINE INITIAL_BLADE_FINITE_ELEMENT
    
    SUBROUTINE INITIAL_BLADE_FINITE_ELEMENT_backup()
    USE GLOBAL_PARAMETERS
    USE ROTOR
    USE AIRFOIL
    IMPLICIT NONE
    !供备份。未使用
    !计算面元中点位置
    !随体坐标系
    
 
    INTEGER :: I,J,K
    REAL(8) :: X_PL,Y_PL,Z_PL  !!在随叶坐标系中IN X5Y5Z5
    REAL(8) :: LTH_PANEL_R,LTH_PANEL_C
    !! r,C,Z
    REAL(8) :: Q_VEC(3),XUNIT(3)
    REAL(8) :: MAX=1.0E31
    REAL(8) :: IB
    REAL(8) :: Z1(3),Z2(3)
    !
    ALLOCATE(XPANEL_UP(3,N_R,N_C))
    ALLOCATE(SPANEL_UP(N_R,N_C))
    ALLOCATE(NPANEL_UP(3,N_R,N_C))
    ALLOCATE(XPANEL_DOWN(3,N_R,N_C))
    ALLOCATE(SPANEL_DOWN(N_R,N_C))
    ALLOCATE(NPANEL_DOWN(3,N_R,N_C))
    
    XUNIT=(/1,0,0/)
    
    !等分单元
    LTH_PANEL_R=(R_BLADE_END-R_BLADE_BEGIN)/N_R
    
    !
    DO IB=1,N_BLADE
        DO K=1,2
            DO I=1,N_R
                LTH_PANEL_C=C_BLADE_R(I)/N_C
                AIRFOIL_POINT_UP=AIRFOIL_POINT_UP*C_BLADE_R(I)
                AIRFOIL_POINT_DOWN=AIRFOIL_POINT_DOWN*C_BLADE_R(I)
                
                DO J=1,N_C 
                    X_PL=R_BLADE_BEGIN+LTH_PANEL_R*(I-0.5)
                    Y_PL=LTH_PANEL_C*(J-0.5)   !原点取在翼型前缘点     
                   ! CALL spline(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),NAIRFOIL,MAX,MAX,Y2A)
                   ! CALL splint(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),Y2A,NAIRFOIL,Y_PL,Z_PL)
            
                    IF (K.EQ.1) THEN
                        CALL spline(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),NAIRFOIL,MAX,MAX,Y2A)
                        CALL splint(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),Y2A,NAIRFOIL,Y_PL,Z_PL)
                        Y_PL=Y_PL-0.25*C_BLADE_R(I)   
                        XPANEL_UP(:,I,J)=(/X_PL,Y_PL,-Z_PL/)
                        !SPANEL_UP(I,J)=LTH_PANEL_R*LTH_PANEL_C
                        
                        Z1(1)=0
                        Z1(2)=Y_PL-C_BLADE_R(I)/(2*N_C)+0.25*C_BLADE_R(I)
                        CALL spline(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),NAIRFOIL,MAX,MAX,Y2A)
                        CALL splint(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),Y2A,NAIRFOIL,Z1(2),Z1(3))
                        Z2(1)=0
                        Z2(2)=Y_PL+C_BLADE_R(I)/(2*N_C)+0.25*C_BLADE_R(I)
                        CALL spline(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),NAIRFOIL,MAX,MAX,Y2A)
                        CALL splint(AIRFOIL_POINT_UP(:,1),AIRFOIL_POINT_UP(:,2),Y2A,NAIRFOIL,Z2(2),Z2(3))
                        
                        SPANEL_UP(I,J)=LTH_PANEL_R*SQRT(SUM((Z2-Z1)*(Z2-Z1)))
                        !SPANEL_UP(I,J)=LTH_PANEL_R*SQRT(DOT_PRODUCT((Z2-Z1),(Z2-Z1)))
                        !WRITE(*,*)
                    ELSE 
                        CALL spline(AIRFOIL_POINT_DOWN(:,1),AIRFOIL_POINT_DOWN(:,2),NAIRFOIL,MAX,MAX,Y2A)
                        CALL splint(AIRFOIL_POINT_DOWN(:,1),AIRFOIL_POINT_DOWN(:,2),Y2A,NAIRFOIL,Y_PL,Z_PL)
                        Y_PL=Y_PL-0.25*C_BLADE_R(I)   
                        XPANEL_DOWN(:,I,J)=(/X_PL,Y_PL,-Z_PL/)
                        !SPANEL_DOWN(I,J)=LTH_PANEL_R*LTH_PANEL_C
                        
                        Z1(1)=0
                        Z1(2)=Y_PL-C_BLADE_R(I)/(2*N_C)+0.25*C_BLADE_R(I)
                        CALL spline(AIRFOIL_POINT_DOWN(:,1),AIRFOIL_POINT_DOWN(:,2),NAIRFOIL,MAX,MAX,Y2A)
                        CALL splint(AIRFOIL_POINT_DOWN(:,1),AIRFOIL_POINT_DOWN(:,2),Y2A,NAIRFOIL,Z1(2),Z1(3))
                        Z2(1)=0
                        Z2(2)=Y_PL+C_BLADE_R(I)/(2*N_C)+0.25*C_BLADE_R(I)
                        CALL spline(AIRFOIL_POINT_DOWN(:,1),AIRFOIL_POINT_DOWN(:,2),NAIRFOIL,MAX,MAX,Y2A)
                        CALL splint(AIRFOIL_POINT_DOWN(:,1),AIRFOIL_POINT_DOWN(:,2),Y2A,NAIRFOIL,Z2(2),Z2(3))
                        
                        SPANEL_DOWN(I,J)=LTH_PANEL_R*SQRT(SUM((Z2-Z1)*(Z2-Z1)))
                        
                    ENDIF
                END DO
                
                AIRFOIL_POINT_UP=AIRFOIL_POINT_UP/C_BLADE_R(I)
                AIRFOIL_POINT_DOWN=AIRFOIL_POINT_DOWN/C_BLADE_R(I)
                
            END DO
        END DO
    END DO
    
    
    
    !
    DO K=1,2
    DO I=1,N_R
        DO J=1,N_C
            IF (K.EQ.1) THEN      
                IF (J.EQ.1) THEN
                    Q_VEC=(/DBLE(0),XPANEL_UP(2,I,J+1)-XPANEL_UP(2,I,J),XPANEL_UP(3,I,J+1)-XPANEL_UP(3,I,J)/)
                ELSE
                    Q_VEC=(/DBLE(0),XPANEL_UP(2,I,J)-XPANEL_UP(2,I,J-1),XPANEL_UP(3,I,J)-XPANEL_UP(3,I,J-1)/)
                END IF
                CALL VECTOR_CROSS3(NPANEL_UP(:,I,J),-XUNIT,Q_VEC)
                CALL UNIT3(NPANEL_UP(:,I,J))            
            ELSE
                IF (J.EQ.1) THEN
                    Q_VEC=(/DBLE(0),XPANEL_DOWN(2,I,J+1)-XPANEL_DOWN(2,I,J),XPANEL_DOWN(3,I,J+1)-XPANEL_DOWN(3,I,J)/)
                ELSE
                    Q_VEC=(/DBLE(0),XPANEL_DOWN(2,I,J)-XPANEL_DOWN(2,I,J-1),XPANEL_DOWN(3,I,J)-XPANEL_DOWN(3,I,J-1)/)
                END IF
                CALL VECTOR_CROSS3(NPANEL_DOWN(:,I,J),XUNIT,Q_VEC)
                CALL UNIT3(NPANEL_DOWN(:,I,J))            
            END IF
        END DO
    END DO
    END DO
    
    END SUBROUTINE INITIAL_BLADE_FINITE_ELEMENT_BACKUP
    
    SUBROUTINE READ_AIRFOIL()
    USE AIRFOIL
    !USE ROTOR,ONLY:C_BLADE
    IMPLICIT NONE
    CHARACTER*60 AIRFOIL_NAME
    CHARACTER*60 COUM
    INTEGER:: I
    REAL(8):: Q
    
    !OPEN(1000,FILE='AIRFOIL.INP')
    OPEN(1000,FILE='AIRFOIL.INP')
    
    READ(1000,*)AIRFOIL_NAME
    READ(1000,*)NAIRFOIL
    ALLOCATE(AIRFOIL_POINT_UP(NAIRFOIL,2))
    ALLOCATE(AIRFOIL_POINT_DOWN(NAIRFOIL,2))
    ALLOCATE(Y2A(NAIRFOIL))
   
    READ(1000,*)COUM
    DO I=1,NAIRFOIL
        READ(1000,*)AIRFOIL_POINT_UP(I,:)
    END DO
    
    READ(1000,*)COUM
    DO I=1,NAIRFOIL
        READ(1000,*)AIRFOIL_POINT_DOWN(I,:)
    END DO
    CLOSE(1000)

    
    !DO I=1,NAIRFOIL
    !    Q=AIRFOIL_POINT_UP(I,1)
    !    AIRFOIL_POINT_UP(I,2)=(0.2969*SQRT(Q)+Q*(-0.126+Q*(-0.3516+Q*(0.2843-0.1015*Q))))/0.2*0.12
    !    AIRFOIL_POINT_DOWN(i,1)=AIRFOIL_POINT_UP(I,1)
    !    AIRFOIL_POINT_DOWN(i,2)=AIRFOIL_POINT_UP(I,2)
    !END DO
    

    END
    
 
    SUBROUTINE OPENFILE()
    USE GLOBAL_PARAMETERS,ONLY:N
    USE xml_data_compution_control
    IMPLICIT NONE
    
    OPEN(18,FILE=TRIM(OUTPUT_FOLDERNAME)//'\TIME_THICKNESS_PRESS.PLT')
    OPEN(19,FILE=TRIM(OUTPUT_FOLDERNAME)//'\TIME_LOADING_PRESS.PLT')
    OPEN(20,FILE=TRIM(OUTPUT_FOLDERNAME)//'\TIME_PRESS.plt')
    OPEN(21,FILE=TRIM(OUTPUT_FOLDERNAME)//'\TIME_SPH.plt')
    OPEN(40,FILE=TRIM(OUTPUT_FOLDERNAME)//'\TEST_OUT1.OUT')
    OPEN(41,FILE=TRIM(OUTPUT_FOLDERNAME)//'\TEST_OUT2.OUT')
    
    OPEN(42,FILE=TRIM(OUTPUT_FOLDERNAME)//'\TEST_OUT3.PLT')
    OPEN(43,FILE=TRIM(OUTPUT_FOLDERNAME)//'\TEST_OUT4.OUT')
    
    
    !WRITE(18,1000)
1000 FORMAT('OBTIME',5X,'PT_NEAR',5X,'PT_FAR',5X,'PT_TOTAL',5X,'PL_NEAR',5X,'PL_FAR',5X,'PL_TOTAL',5X,'PRESS')
     
    WRITE(18,*)' TITLE = "TIME_THICKNESS_PRESS" '
    WRITE(18,1005)
    WRITE(18,*)' ZONE I=',N, ',F=POINT '
1005 FORMAT('VARIABLES = "X", "PT_FAR","PT_NEAR","PT_TOTAL"  ')    
    
    WRITE(19,*)' TITLE = "WAKE OF HELICOPTER" '
    WRITE(19,1010)
    WRITE(19,*)' ZONE I=',N, ',F=POINT '
1010 FORMAT('VARIABLES = "X", "PL_FAR","PL_NEAR","PL_TOTAL"  ')    
     
    WRITE(20,*)' TITLE = "WAKE OF HELICOPTER" '
    WRITE(20,*)' VARIABLES = "X", "PT","PL","P_TOTAL" '
    WRITE(20,*)' ZONE I=',N, ',F=POINT '
    
    
    WRITE(21,*)' TITLE = "WAKE OF HELICOPTER" '
    WRITE(21,*)' VARIABLES = "X", "SPH_PT","SPH_PL","SPH_P_TOTAL" '
    WRITE(21,*)' ZONE I=',N, ',F=POINT '
    
    WRITE(50,*)' TITLE = "WAKE OF HELICOPTER" '
    WRITE(50,1015)
    WRITE(50,*)' ZONE I=',N, ',F=POINT '
1015 FORMAT('VARIABLES = "X", "PL1","PL2","PL3","PL4"  ')    
!     
    END SUBROUTINE
    
    
    SUBROUTINE INIT_ACP_HIS()
    USE GLOBAL_PARAMETERS,ONLY:N
    USE ACP_HIS
    IMPLICIT NONE
    ALLOCATE(TIME_HIS(N))
    ALLOCATE(PL_HIS(N))
    ALLOCATE(PT_HIS(N))
    ALLOCATE(P_TOTAL_HIS(N))
    END SUBROUTINE
    
    
    SUBROUTINE INIT_SPEC()
    USE SPEC
    USE GLOBAL_PARAMETERS
    USE ACP_HIS
    USE PARAMETEERS
    USE ROTOR
    USE ACP_HIS
    !USE xml_data_noise_parameters
    use xml_data_noise_input_parameters
    use xml_data_noise_rotor_parameters
    use xml_data_noise_solver_parameters
    
    USE SPEC
    IMPLICIT NONE
    
    INTEGER::I,J
    ALLOCATE(PT_HIS_EXPEND(N*N_WINDOW))
    ALLOCATE(PL_HIS_EXPEND(N*N_WINDOW))
    ALLOCATE(P_TOTAL_HIS_EXPEND(N*N_WINDOW))
    
    N_SPEC=N/NPEROID_INPUT*N_WINDOW
    !write(*,*)n_spec
    !DO I=1,N_SPEC
    !    J=mod(i,N/NPEROID_INPUT)
    !    if(j.eq.0)j=N/NPEROID_INPUT
    !    PT_HIS_EXPEND(I)=PT_HIS(J)
    !    PL_HIS_EXPEND(I)=PL_HIS(J)
    !    P_TOTAL_HIS_EXPEND(I)=P_TOTAL_HIS(J)
    !END DO
    
    
    
    
    
    ALLOCATE(PT_SPEC(N_SPEC/2+1))
    ALLOCATE(PL_SPEC(N_SPEC/2+1))
    ALLOCATE(P_TOTAL_SPEC(N_SPEC/2+1))
    
    ALLOCATE(SPLT_SPEC(N_SPEC/2+1))
    ALLOCATE(SPLL_SPEC(N_SPEC/2+1))
    ALLOCATE(SPL_TOTAL_SPEC(N_SPEC/2+1))
    
    ALLOCATE(PT_SPEC_A(N_SPEC/2+1))
    ALLOCATE(PL_SPEC_A(N_SPEC/2+1))
    ALLOCATE(P_TOTAL_SPEC_A(N_SPEC/2+1))
    
    ALLOCATE(SPLT_SPEC_A(N_SPEC/2+1))
    ALLOCATE(SPLL_SPEC_A(N_SPEC/2+1))
    ALLOCATE(SPL_TOTAL_SPEC_A(N_SPEC/2+1))
    
    END SUBROUTINE