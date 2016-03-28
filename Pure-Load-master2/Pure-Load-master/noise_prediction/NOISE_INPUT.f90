


    SUBROUTINE system_input()
    !use xml_data_noise_input_parameters
    use xml_data_noise_input_parameters
    use xml_data_noise_rotor_parameters
    use xml_data_noise_solver_parameters
    USE DFPORT
    use xml_data_compution_control

    implicit none
    character(len=80) title
    character(len=80) coum(10)
    !character(len=80) lmx
    
    integer istatus
    character*256 NewFolderName1,NewFolderName2
    integer i
    call read_xml_file_noise_input_parameters( 'noise_input_parameters.XML' )
    !call read_xml_file_noise_input_parameters( 'noise_input_parameters.XML' )
    OUTPUT_FOLDERNAME=noise_file_name
    call read_xml_file_noise_rotor_parameters(trim(noise_rotor_file_name)//'.xml')
    call read_xml_file_noise_solver_parameters(trim(noise_solver_file_name)//'.xml')
   ! write(*,*)trim(OUTPUT_FOLDERNAME)
   !write(*,*)RHO_INPUT,VSPEED_INPUT
   ! write(*,*)N_R_INPUT
    
    
    
  
    !
    !
    istatus=System("Md "//TRIM(OUTPUT_FOLDERNAME)) !建第一层文件夹
    call write_xml_file_noise_rotor_parameters(TRIM(OUTPUT_FOLDERNAME)//'\noise_rotor_OUT.xml')
    call write_xml_file_noise_solver_parameters(TRIM(OUTPUT_FOLDERNAME)//'\noise_solver_OUT.xml')
    call write_xml_file_noise_input_parameters(TRIM(OUTPUT_FOLDERNAME)//'\noise_input_OUT.xml')
    !call write_xml_file_NOISE_parameters( TRIM(OUTPUT_FOLDERNAME)//'\INPUT_PARAMETERS_OUT.xml' )
    !call write_xml_file_NOISE_parameters( TRIM(OUTPUT_FOLDERNAME)//'\INPUT_PARAMETERS_OUT.xml' )
    

    


    END 



