
    SUBROUTINE GRAPHICS()
    USE PARAMETEERS
    USE GLOBAL_PARAMETERS
    USE PARAMETEERS
    USE ROTOR
    USE xml_data_compution_control
    USE ACP_HIS
    !USE xml_data_noise_parameters
    use xml_data_noise_input_parameters
    use xml_data_noise_rotor_parameters
    use xml_data_noise_solver_parameters
    
    USE DISLIN
    !≈‰÷√∑Ω∑®£∫http://www.mps.mpg.de/1758446/faq_kap4#q_1
    
    IMPLICIT NONE
    INTEGER::I
    REAL(KIND=4)::A(N),B(N),C(N)
    REAL(KIND=4)::XMIN,XMAX,YMIN,YMAX,DX,DY
    character(len=80):: CBUF
    !character(len=60):: CBUF2
  
    A=REAL(TIME_HIS/TIME_PEROID)
    
    CALL METAFL('XWIN')
    CALL SCRMOD('AUTO')
    CALL DISINI()
    CALL HEIGHT(25)
    CALL TEXMOD ('ON')
    CALL PAGERA()
    CALL COMPLX()
    !CALL AXSLEN(1100,600)
    CALL HNAME(25)
    
    !CALL AXSPOS(300,850)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    CALL TITLIN('TITLE 1',1)
    !CALL TITLIN('TITLE 2',2)
    CALL TITLIN('TITLE 3',3)
    CALL COLOR('FORE')
    
    
    CALL NAME('TIME_HIS/TIME_PEROID','X')
    CALL NAME('ACOUSTICS PB','Y')
    
    CALL LABDIG(1,'X')
    CALL LABDIG(2,'Y')
    CALL TICKS(4,'X')
    CALL TICKS(2,'Y')
    
   
    
    xmin = minval(TIME_HIS/TIME_PEROID)
    xmax = maxval(TIME_HIS/TIME_PEROID)
    ymin = -150
    ymax = 50
    
    dx = (xmax-xmin)/8.0
    dy = (ymax-ymin)/8.0 
    CALL GRAF(xmin,xmax,xmin,dx,ymin,ymax,ymin,dy)
    !CALL LEGVAL (0.6,'SYMBOL')

    CALL COLOR('FORE')
    CALL TITLE()
    
    CALL COLOR('GREEN')
    CALL INCMRK(-1)     !selects line or symbol mode
    CALL MARKER(21)     !sets the symbols plotted
    CALL HSYMBL(10)     !sets the sIZE plotted
    CALL CURVE(REAL(TIME_HIS/TIME_PEROID),REAL(PT_HIS),N)
    
    CALL COLOR('RED')
    CALL INCMRK(-1)     !selects line or symbol mode
    CALL MARKER(21)     !sets the symbols plotted
    CALL HSYMBL(10)     !sets the sIZE plotted
    CALL CURVE(REAL(TIME_HIS/TIME_PEROID),REAL(PL_HIS),N)
    
    
    CALL COLOR('BLUE')
    CALL INCMRK(-1)     !selects line or symbol mode
    CALL MARKER(21)     !sets the symbols plotted
    CALL HSYMBL(10)     !sets the sIZE plotted
    CALL CURVE(REAL(TIME_HIS/TIME_PEROID),REAL(P_TOTAL_HIS),N)
    
  
    
    
    
    CALL LEGINI(CBUF,4,20)
    CALL LEGLIN(CBUF,'PT_HIS',1)
    CALL LEGLIN(CBUF,'PL_HIS',2)
    CALL LEGLIN(CBUF,'P_TOTAL_HIS',3)
    
    !CALL LEGTIT('XX')
    CALL COLOR('FORE')
    CALL LEGEND(CBUF,3)


    
    CALL ENDGRF()
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    CALL DISFIN()
    
    END SUBROUTINE