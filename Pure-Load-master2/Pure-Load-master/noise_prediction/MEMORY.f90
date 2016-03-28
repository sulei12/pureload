    SUBROUTINE INIT_MEMORY()
    USE ROTOR
    USE AIRFOIL
    USE ACP_HIS
    USE SPEC
    IMPLICIT NONE
    
    
    END SUBROUTINE
    
    
    SUBROUTINE DE_MEMORY()
    USE ROTOR
    USE AIRFOIL
    USE ACP_HIS
    USE SPEC
    IMPLICIT NONE
    
    CLOSE(18)
    CLOSE(19)
    CLOSE(20)
    CLOSE(21)
    CLOSE(40)
    
    DEALLOCATE(C_BLADE_R)
    DEALLOCATE(BLADE_SPACING)
    
    DEALLOCATE(THETA_BLADE)
    DEALLOCATE(BETA_BLADE)
    DEALLOCATE(DBETA_BLADE)
    DEALLOCATE(D2BETA_BLADE)
    
    DEALLOCATE(PT_TIME)
    DEALLOCATE(PT_R)
    !DEALLOCATE(FORCE_CELL)
    
    !DEALLOCATE(V_INDUCE)
    DEALLOCATE(XPANEL_UP)
    DEALLOCATE(SPANEL_UP)
    DEALLOCATE(NPANEL_UP)
    DEALLOCATE(XPANEL_DOWN)
    DEALLOCATE(SPANEL_DOWN)
    DEALLOCATE(NPANEL_DOWN)
    
    DEALLOCATE(AIRFOIL_POINT_UP)
    DEALLOCATE(AIRFOIL_POINT_DOWN)
    DEALLOCATE(Y2A)
    
    DEALLOCATE(TIME_HIS)
    DEALLOCATE(PL_HIS)
    DEALLOCATE(PT_HIS)
    DEALLOCATE(P_TOTAL_HIS)
    
    
    DEALLOCATE(PT_SPEC)
    DEALLOCATE(PL_SPEC)
    DEALLOCATE(P_TOTAL_SPEC)
    
    DEALLOCATE(SPLT_SPEC)
    DEALLOCATE(SPLL_SPEC)
    DEALLOCATE(SPL_TOTAL_SPEC)
  
    DEALLOCATE(PT_SPEC_A)
    DEALLOCATE(PL_SPEC_A)
    DEALLOCATE(P_TOTAL_SPEC_A)
    
    DEALLOCATE(SPLT_SPEC_A)
    DEALLOCATE(SPLL_SPEC_A)
    DEALLOCATE(SPL_TOTAL_SPEC_A)
    
    
    DEALLOCATE(LOADING_MATRIX)
    DEALLOCATE(PSI_ARRAY)
        
    DEALLOCATE(PT_HIS_EXPEND)
    DEALLOCATE(PL_HIS_EXPEND)
    DEALLOCATE(P_TOTAL_HIS_EXPEND)
    
    WRITE(*,*)
    END