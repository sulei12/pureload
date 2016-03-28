

    SUBROUTINE ANALYSIS_SPECTRUM()

    USE SPEC
    USE ACP_HIS
    USE GLOBAL_PARAMETERS
    
      !USE XML_DATA_HELICOPTER_PARAMETERS
    USE XML_DATA_NOISE_ROTOR_PARAMETERS
    USE XML_DATA_NOISE_INPUT_PARAMETERS
    USE XML_DATA_NOISE_SOLVER_PARAMETERS
    USE PARAMETEERS
    
    IMPLICIT NONE
    INTEGER::I,J
    REAL(8)::TOTAL_TIME,DET_TIME
    REAL(8)::S_L,S_T,S
    S_L=0;S_T=0;S=0
    
    
    !CALL INIT_SPEC

    !N_SPEC=N/NPEROID_INPUT*N_WINDOW
    !WRITE(*,*)N_SPEC
    DO I=1,N_SPEC
        J=MOD(I,N/NPEROID_INPUT)
        IF(J.EQ.0)J=N/NPEROID_INPUT
        PT_HIS_EXPEND(I)=PT_HIS(J)
        PL_HIS_EXPEND(I)=PL_HIS(J)
        P_TOTAL_HIS_EXPEND(I)=P_TOTAL_HIS(J)
    END DO
    
    CALL DFT(PT_HIS_EXPEND,PT_SPEC,N_SPEC,TIME_PEROID*N_WINDOW,FREQ_SAMPLING)
    CALL DFT(PL_HIS_EXPEND,PL_SPEC,N_SPEC,TIME_PEROID*N_WINDOW,FREQ_SAMPLING)
    CALL DFT(P_TOTAL_HIS_EXPEND,P_TOTAL_SPEC,N_SPEC,TIME_PEROID*N_WINDOW,FREQ_SAMPLING)
    
    FREQ_SAMPLING=N_SPEC/(N_WINDOW)/TIME_PEROID
    
    DO I=1,N_SPEC/2+1
        IF(ABS(PT_SPEC(I)).LE.P_REF)PT_SPEC(I)=P_REF
        IF(ABS(PL_SPEC(I)).LE.P_REF)PL_SPEC(I)=P_REF
        IF(ABS(P_TOTAL_SPEC(I)).LE.P_REF)P_TOTAL_SPEC(I)=P_REF
        SPLT_SPEC(I)=20*LOG10(ABS(PT_SPEC(I)/P_REF))
        SPLL_SPEC(I)=20*LOG10(ABS(PL_SPEC(I)/P_REF))
        SPL_TOTAL_SPEC(I)=20*LOG10(ABS(P_TOTAL_SPEC(I)/P_REF))
    END DO
    
    TOTAL_TIME=TIME_PEROID*N_WINDOW
    DET_TIME=TOTAL_TIME/N_SPEC
    
    ! FOR TOTAL
    DO I=1,N_SPEC
        S_T=S_T+PT_HIS_EXPEND(I)*PT_HIS_EXPEND(I)*DET_TIME
        S_L=S_L+PL_HIS_EXPEND(I)*PL_HIS_EXPEND(I)*DET_TIME        
        S=S+P_TOTAL_HIS_EXPEND(I)*P_TOTAL_HIS_EXPEND(I)*DET_TIME
    END DO
    
    S_L=S_L/TOTAL_TIME
    S_T=S_T/TOTAL_TIME
    S=S/TOTAL_TIME
    SPLT=20*LOG10(ABS(SQRT(S_T)/P_REF))
    SPLL=20*LOG10(ABS(SQRT(S_L)/P_REF))
    SPL=20*LOG10(ABS(SQRT(S)/P_REF))

    END SUBROUTINE
    
    SUBROUTINE A_WEIGHTED_SPL()
    ! FOR SPLA: A WEIGHTED SOUND LEVAL
    ! DATA FROM WIKIPADIA OR PPT OF LISONG
    
    USE SPEC
    USE ACP_HIS
    USE GLOBAL_PARAMETERS
    USE XML_DATA_NOISE_ROTOR_PARAMETERS
    USE XML_DATA_NOISE_INPUT_PARAMETERS
    USE XML_DATA_NOISE_SOLVER_PARAMETERS
    USE PARAMETEERS
    
    IMPLICIT NONE
    INTEGER :: I,J
    REAL(8):: A_OST
    REAL(8):: S_T2,S_L2,S2
    S_T2=0;S_L2=0;S2=0
    write(*,*)
    DO I=1,N_SPEC/2+1
        PT_SPEC_A(I)=PT_SPEC(I)
        PL_SPEC_A(I)=PL_SPEC(I)
        P_TOTAL_SPEC_A(I)=P_TOTAL_SPEC(I)
    END DO
    
    
     DO I=1,N_SPEC/2+1
        CALL A_GAIN(FREQ_SAMPLING/N_SPEC*(I-1),A_OST)
        WRITE(*,*)FREQ_SAMPLING/N_SPEC*(I-1),A_OST
        !A_OST=0
        
        IF((PT_SPEC_A(I)).LE.P_REF)PT_SPEC_A(I)=P_REF
        IF((PL_SPEC_A(I)).LE.P_REF)PL_SPEC_A(I)=P_REF
        IF((P_TOTAL_SPEC_A(I)).LE.P_REF)P_TOTAL_SPEC_A(I)=P_REF
        
        SPLT_SPEC_A(I)=20*LOG10(ABS(PT_SPEC_A(I)/P_REF))+A_OSt
        SPLL_SPEC_A(I)=20*LOG10(ABS(PL_SPEC_A(I)/P_REF))+A_OST
        SPL_TOTAL_SPEC_A(I)=20*LOG10(ABS(P_TOTAL_SPEC_A(I)/P_REF))+A_OST
        
        iF((SPLT_SPEC_A(I)).LE.0.0)SPLT_SPEC_A(I)=0.0
        IF((SPLL_SPEC_A(I)).LE.0.0)SPLL_SPEC_A(I)=0.0
        IF((SPL_TOTAL_SPEC_A(I)).LE.0.0)SPL_TOTAL_SPEC_A(I)=0.0
        
       
        PT_SPEC_A(I)=10**(SPLT_SPEC_A(I)/20)*p_ref
        PL_SPEC_A(I)=10**(SPLL_SPEC_A(I)/20)*p_ref
        P_TOTAL_SPEC_A(I)=10**(SPL_TOTAL_SPEC_A(I)/20)*p_ref
     END DO
     
     
       
    
    DO I=1,N_SPEC/2+1
        S_T2=S_T2+PT_SPEC_A(I)*PT_SPEC_A(I)/(TIME_PEROID*N_WINDOW)
        S_L2=S_L2+PL_SPEC_A(I)*PL_SPEC_A(I)/(TIME_PEROID*N_WINDOW)
        S2=S2+P_TOTAL_SPEC_A(I)*P_TOTAL_SPEC_A(I)/(TIME_PEROID*N_WINDOW)
    END DO
    DO I=2,N_SPEC/2
        S_T2=S_T2+PT_SPEC_A(I)*PT_SPEC_A(I)/(TIME_PEROID*N_WINDOW)
        S_L2=S_L2+PL_SPEC_A(I)*PL_SPEC_A(I)/(TIME_PEROID*N_WINDOW)
        S2=S2+P_TOTAL_SPEC_A(I)*P_TOTAL_SPEC_A(I)/(TIME_PEROID*N_WINDOW)
    END DO
    S_T2=S_T2*TIME_PEROID*N_WINDOW/N_SPEC*N
    S_L2=S_L2*TIME_PEROID*N_WINDOW/N_SPEC*N
    S2=S2*TIME_PEROID*N_WINDOW/N_SPEC*N
    
    SPLT_A=20*LOG10(ABS(SQRT(S_T2)/P_REF))
    SPLL_A=20*LOG10(ABS(SQRT(S_L2)/P_REF))
    SPL_A=20*LOG10(ABS(SQRT(S2)/P_REF))
    
    END SUBROUTINE
    
    
    SUBROUTINE A_GAIN(FREQ,A_OST)
    !GET A-GAIN OFFSET
    IMPLICIT NONE
    REAL(8),INTENT(IN)::FREQ
    REAL(8),INTENT(OUT)::A_OST
    REAL(8)::COEF(7)
    REAL(8)::LGFREQ
    INTEGER :: I
    DATA COEF /-0.379345526262496,6.059536213467178,-39.850192680920210,1.395069161574801E+02,-2.889260182652540E+02,3.770400024199245E+02,-2.655814852518744E+02/
    A_OST=COEF(7);
    if (FREQ .lt.10)then 
        LGFREQ=LOG10(10.0)
        A_OST=-200
        return
    else
        LGFREQ=LOG10(FREQ);
    end if
    
    DO I=6,1,-1
        A_OST=LGFREQ*COEF(I)+A_OST
        LGFREQ=LGFREQ*LOG10(FREQ)
    END DO
    END SUBROUTINE 