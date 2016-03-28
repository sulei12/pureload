    SUBROUTINE OUTPUT()
    IMPLICIT NONE
    !CALL OUTPUT_CHECK
   
    END SUBROUTINE OUTPUT
    
    SUBROUTINE OUTPUT_CHECK()
    USE GLOBAL_PARAMETERS
    USE ROTOR
    USE xml_data_compution_control
    IMPLICIT NONE
    INTEGER :: I,J,IB
    OPEN(1024,FILE=TRIM(OUTPUT_FOLDERNAME)//'\CHECK_BLADE_INFO.OUT')
    WRITE(1024,*)
    WRITE(1024,*)'---------------CHECK FOR COMPUTING PARAMETERS---------------'
    WRITE(1024,*)'---------------CHECK FOR INPUT DATA---------------'
    WRITE(1024,*)
    !WRITE(1024,100)R_BLADE_BEGIN,R_BLADE_END,C_BLADE
    WRITE(1024,*)'  R_BLADE_BEGIN=',R_BLADE_BEGIN
    WRITE(1024,*)'  R_BLADE_END=',R_BLADE_END
    WRITE(1024,*)'  C_BLADE='!,C_BLADE
    WRITE(1024,*)'  OMEGA=',OMEGA
    WRITE(1024,*)'---------------CHECK FOR BLADE FEM---------------'
    WRITE(1024,*)'N_R=',N_R,'N_C=',N_C
    WRITE(1024,*)'XPANEL_UP:'
    DO I=1,N_R
        DO J=1,N_C
            WRITE(1024,99)XPANEL_UP(:,I,J)
        END DO
    END DO
    WRITE(1024,*)'XPANEL_DOWN:'
    DO I=1,N_R
        DO J=1,N_C
            WRITE(1024,99)XPANEL_DOWN(:,I,J)
        END DO
    END DO
    WRITE(1024,*)'SPANEL_UP:'
    DO I=1,N_R
        DO J=1,N_C
            WRITE(1024,'(F12.6)')SPANEL_UP(I,J)
        END DO
    END DO
    WRITE(1024,*)'SPANEL_DOWN:'
    DO I=1,N_R
        DO J=1,N_C
            WRITE(1024,'(F12.6)')SPANEL_DOWN(I,J)
        END DO
    END DO
    WRITE(1024,*)'NPANEL_UP:'
    DO I=1,N_R
        DO J=1,N_C
            WRITE(1024,99)NPANEL_UP(:,I,J)
        END DO
    END DO
    WRITE(1024,*)'NPANEL_DOWN:'
    DO I=1,N_R
        DO J=1,N_C
            WRITE(1024,99)NPANEL_DOWN(:,I,J)
        END DO
    END DO
    CLOSE (1024)
99  FORMAT(3F12.6)    
100 FORMAT('  R_BLADE_BEGIN=',F/,'  R_BLADE_BEGIN=',F/,'  C_BLADE=',F/)   
    !FORMAT('  R_BLADE_BEGIN=',F12.6/,'  R_BLADE_BEGIN=',F12.6/,'  C_BLADE=',F12.6/)   
    END SUBROUTINE
    
    SUBROUTINE TESTOUT()
    USE ROTOR
    USE COOR_TRAN
    USE GLOBAL_PARAMETERS
    USE PARAMETEERS
    
    IMPLICIT NONE
    INTEGER::I
    !WRITE(40,*)'OBTIME:',OBTIME,'RESTIME:',RESTIME,'PSI:',PSI_DEG
    !WRITE(40,*)'LOAD:'
    END SUBROUTINE
    
    SUBROUTINE OUTPUT_HIS_P()
    USE ACP_HIS
    USE GLOBAL_PARAMETERS
    USE xml_data_compution_control
    USE SPEC
    IMPLICIT NONE
    INTEGER::I
    OPEN(100,FILE=TRIM(OUTPUT_FOLDERNAME)//'\ACP_HIS.PLT')
    WRITE(100,*)' TITLE = "AEROACOUSTICS" '
    WRITE(100,100)
    WRITE(100,*)' ZONE I=',N, ',F=POINT '
    
    DO I=1,N
     WRITE(100,105) TIME_HIS(I)/TIME_PEROID,PT_HIS(I),PL_HIS(I),P_TOTAL_HIS(I)
     !WRITE(100,105) TIME_HIS(I),PT_HIS(I),PL_HIS(I),P_TOTAL_HIS(I)
    END DO
    CLOSE(100)
    
100 FORMAT('VARIABLES = "TIME", "PT","PL","P_TOTAL"  ')    
105 FORMAT(4(F12.6,3X))     
    END SUBROUTINE
    
    
    SUBROUTINE SPTM_OUTPUT()
    USE ACP_HIS
    USE GLOBAL_PARAMETERS
    USE xml_data_compution_control
    use xml_data_noise_input_parameters
    use xml_data_noise_rotor_parameters
    use xml_data_noise_solver_parameters
    USE SPEC
    IMPLICIT NONE
    INTEGER I
    
    OPEN(100,FILE=TRIM(OUTPUT_FOLDERNAME)//'\P-SPTM.PLT')
    WRITE(100,*)' TITLE = "SPECTRUM" '
    WRITE(100,*)' VARIABLES = "FREQUENCE","PT","PL","P_TOTAL"  '
    WRITE(100,*)' ZONE I=',N_SPEC/2+1, ',F=POINT '
    DO I=1,N_SPEC/2+1
        WRITE(100,205)FREQ_SAMPLING/N_SPEC*(I-1),PT_SPEC(I),PL_SPEC(I),P_TOTAL_SPEC(I)
    END DO
    CLOSE (100)
    
    OPEN(100,FILE=TRIM(OUTPUT_FOLDERNAME)//'\SPL-SPTM.PLT')
    WRITE(100,*)' TITLE = "SPECTRUM" '
    WRITE(100,*)' VARIABLES = "FREQUENCE","SPL-T","SPL-L","SPL" ,"SPL-T_A","SPL-L_A","SPL_A"  '
    WRITE(100,*)' ZONE I=',N_SPEC/2+1, ',F=POINT '
    DO I=1,N_SPEC/2+1
        WRITE(100,205)FREQ_SAMPLING/N_SPEC*(I-1),SPLT_SPEC(I),SPLL_SPEC(I),SPL_TOTAL_SPEC(I),SPLT_SPEC_A(I),SPLL_SPEC_A(I),SPL_TOTAL_SPEC_A(I)
    END DO
    CLOSE (100)
    
    
    
    !OPEN(100,FILE=TRIM(OUTPUT_FOLDERNAME)//'\SPL-TOTAL.DAT')
    !write(100,*)OUTPUT_FOLDERNAME
    !WRITE(100,*)'SPL_T  ', 'SPL_L   ','SPL_TOTAL'
    !WRITE(*,*)
    !WRITE(*,*)'SPL_T  ', 'SPL_L   ','SPL_TOTAL'
    !
    !WRITE(100,206) SPLT,SPLL,SPL
    !WRITE(*,206) SPLT,SPLL,SPL
    !CLOSE (100)
    
205 FORMAT(7(F12.6,3X))      
206 FORMAT(3(F5.1,3X))    
    END SUBROUTINE
    
    
    
    
    
    SUBROUTINE OUTPUT_GRID()
    USE ACP_HIS
    USE GLOBAL_PARAMETERS
    USE xml_data_compution_control
    USE ROTOR
    use xml_data_noise_input_parameters
    use xml_data_noise_rotor_parameters
    use xml_data_noise_solver_parameters
    USE SPEC
    
    IMPLICIT NONE
    INTEGER I,J
    OPEN(1000,FILE=TRIM(OUTPUT_FOLDERNAME)//'\GRID_PER_BLADE.DAT')
    WRITE(1000,*)' TITLE = "GRID_PER_BLADE" '
    WRITE(1000,*)' VARIABLES = "X","Y","Z"  '
    WRITE(1000,1001)N_R*N_C,(N_R-1)*(N_C-1)
1001 FORMAT('ZONE, N=',I,',E=',I,', F=FEPOINT, ET=QUADRILATERAL') 
     DO I=1,N_R
         DO J=1,N_C
            WRITE(1000,*)XPANEL_UP(:,I,J)
         END DO
     END DO
     
     DO I=1,N_R-1
         DO J=1,N_C-1
             WRITE(1000,*)(I-1)*N_C+J,(I-1)*N_C+J+1,I*N_C+J+1,I*N_C+J
         END DO
     END DO
     
     
 
    
    CLOSE (100)
    
205 FORMAT(4(F12.6,3X))      
206 FORMAT(3(F5.1,3X))    
    
    
    END SUBROUTINE