 
    PROGRAM MAIN_AEROACOUSTICS
    !USE GLOBAL_PARAMETERS
    USE GLOBAL_PARAMETERS
    USE PARAMETEERS
    USE ROTOR
    USE xml_data_compution_control
    USE ACP_HIS
    !USE xml_data_noise_parameters
    use xml_data_noise_input_parameters
    use xml_data_noise_rotor_parameters
    use xml_data_noise_solver_parameters
    
    USE SPEC
    
    IMPLICIT NONE
    CHARACTER ::CC=achar(13) 
    INTEGER :: I,j
    REAL(8)::A
    REAL(8):: TEST1,TEST2
    
    
  
     
    
    CALL system_input() 
    
    N=N_TIME_INPUT
    
    
    OBTIME=1
    CALL INIT_ACP_HIS()
    CALL INITIAL_DATA()
    !TIME_PEROID=2*PI/N_BLADE/OMEGA
    TIME_PEROID=2*PI/OMEGA/n_blade
    call OUTPUT_GRID()
    
    OPEN(107,FILE=TRIM(OUTPUT_FOLDERNAME)//'\data_SPL-TOTAL.PLT')
    !write(100,*)OUTPUT_FOLDERNAME
    !WRITE(100,*)'X_VISIT','SPL_T  ', 'SPL_L   ','SPL_TOTAL'
    WRITE(*,*)
    
    WRITE(107,*)' TITLE = "SPECTRUM" '
    WRITE(107,*)' VARIABLES = "X","Y","Z","SPL-T","SPL-L","SPL"  '
    WRITE(107,*)' ZONE I=',100, ',F=POINT '
    
    DO I=1,1
        do j=1,1
        !X_VISIT(1)=i-50
        !X_VISIT(2)=j-50
        !X_VISIT(3)=0

        DO OBTIME=1,N 
            T_VISIT=(OBTIME-1)*TIME_PEROID/N*NPEROID_INPUT
            TIME_HIS(OBTIME)=T_VISIT        
            WRITE(*,100)CC,1.0*REAL(OBTIME)/N*100
            !write(*,*)X_VISIT
            CALL FWHsolver()
        END DO
        write(*,*)
        CALL OUTPUT_HIS_P
        CALL ANALYSIS_SPECTRUM()
        CALL A_WEIGHTED_SPL()
        CALL SPTM_OUTPUT()   
        
        WRITE(*,*)'X_VISIT  ','SPL_T  ', 'SPL_L   ','SPL_TOTAL','SPL_T_A  ', 'SPL_L_A   ','SPL_TOTAL_A'
        WRITE(107,206)X_VISIT(1),X_VISIT(2),X_VISIT(3),SPLT,SPLL,SPL,SPLT_A,SPLL_A,SPL_A
        WRITE(*,206)X_VISIT(1),X_VISIT(2),X_VISIT(3),SPLT,SPLL,SPL,SPLT_A,SPLL_A,SPL_A
        
        end do
    END DO
    
    
    
    
    
    CLOSE (107)
    
206 FORMAT(9(F5.1,3X))   
100 FORMAT(A,'THE PROGRAM HAS FINISHED:',F15.5,'% ',$)    
    
    write(*,*)
    write(*,*)
    write(*,*)
    !test1=0
    !call A_GAIN(test1,test2)
    !write(*,*)test2
    
    CALL DE_MEMORY
    END PROGRAM
    
    
    