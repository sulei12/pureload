module xml_data_noise_rotor_parameters
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_
   real                                            :: T_VISIT_INPUT
   real, dimension(:), pointer                     :: X_VISIT_INPUT => null()
   real, dimension(:), pointer                     :: V_INF_INPUT => null()

type rotortype
   integer                                         :: NBLADE_INPUT
   real, dimension(:), pointer                     :: BLADE_SPACING_INPUT => null()
   real                                            :: R_BLADE_BEGIN_INPUT
   real                                            :: R_BLADE_END_INPUT
   integer                                         :: N_CONTROL_INPUT
   real, dimension(:), pointer                     :: R_CONTROL_INPUT => null()
   real, dimension(:), pointer                     :: C_BLADE_INPUT => null()
   real                                            :: AL_ROTOR_INPUT
   real                                            :: OMEGA_INPUT
   real, dimension(:), pointer                     :: BETA_INPUT => null()
   real, dimension(:), pointer                     :: THET_INPUT => null()
   real                                            :: THETT
end type rotortype
   type(rotortype)                                 :: rotor1
   real                                            :: RHO_INPUT
   real                                            :: VSPEED_INPUT
contains
subroutine read_xml_type_rotortype_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(rotortype), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(rotortype), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_rotortype( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_rotortype_array

subroutine read_xml_type_rotortype( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(rotortype), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                         :: has_NBLADE_INPUT
   logical                                         :: has_BLADE_SPACING_INPUT
   logical                                         :: has_R_BLADE_BEGIN_INPUT
   logical                                         :: has_R_BLADE_END_INPUT
   logical                                         :: has_N_CONTROL_INPUT
   logical                                         :: has_R_CONTROL_INPUT
   logical                                         :: has_C_BLADE_INPUT
   logical                                         :: has_AL_ROTOR_INPUT
   logical                                         :: has_OMEGA_INPUT
   logical                                         :: has_BETA_INPUT
   logical                                         :: has_THET_INPUT
   logical                                         :: has_THETT
   has_NBLADE_INPUT                     = .false.
   has_BLADE_SPACING_INPUT              = .false.
   has_R_BLADE_BEGIN_INPUT              = .false.
   has_R_BLADE_END_INPUT                = .false.
   has_N_CONTROL_INPUT                  = .false.
   has_R_CONTROL_INPUT                  = .false.
   has_C_BLADE_INPUT                    = .false.
   has_AL_ROTOR_INPUT                   = .false.
   has_OMEGA_INPUT                      = .false.
   has_BETA_INPUT                       = .false.
   has_THET_INPUT                       = .false.
   has_THETT                            = .false.
   call init_xml_type_rotortype(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('NBLADE_INPUT')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%NBLADE_INPUT, has_NBLADE_INPUT )
      case('BLADE_SPACING_INPUT')
         call read_xml_real_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%BLADE_SPACING_INPUT, has_BLADE_SPACING_INPUT )
      case('R_BLADE_BEGIN_INPUT')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%R_BLADE_BEGIN_INPUT, has_R_BLADE_BEGIN_INPUT )
      case('R_BLADE_END_INPUT')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%R_BLADE_END_INPUT, has_R_BLADE_END_INPUT )
      case('N_CONTROL_INPUT')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%N_CONTROL_INPUT, has_N_CONTROL_INPUT )
      case('R_CONTROL_INPUT')
         call read_xml_real_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%R_CONTROL_INPUT, has_R_CONTROL_INPUT )
      case('C_BLADE_INPUT')
         call read_xml_real_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%C_BLADE_INPUT, has_C_BLADE_INPUT )
      case('AL_ROTOR_INPUT')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%AL_ROTOR_INPUT, has_AL_ROTOR_INPUT )
      case('OMEGA_INPUT')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%OMEGA_INPUT, has_OMEGA_INPUT )
      case('BETA_INPUT')
         call read_xml_real_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%BETA_INPUT, has_BETA_INPUT )
      case('THET_INPUT')
         call read_xml_real_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%THET_INPUT, has_THET_INPUT )
      case('THETT')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%THETT, has_THETT )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // TRIM(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_NBLADE_INPUT ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on NBLADE_INPUT')
   endif
   if ( .not. has_BLADE_SPACING_INPUT ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on BLADE_SPACING_INPUT')
   endif
   if ( .not. has_R_BLADE_BEGIN_INPUT ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on R_BLADE_BEGIN_INPUT')
   endif
   if ( .not. has_R_BLADE_END_INPUT ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on R_BLADE_END_INPUT')
   endif
   if ( .not. has_N_CONTROL_INPUT ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on N_CONTROL_INPUT')
   endif
   if ( .not. has_R_CONTROL_INPUT ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on R_CONTROL_INPUT')
   endif
   if ( .not. has_C_BLADE_INPUT ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on C_BLADE_INPUT')
   endif
   if ( .not. has_AL_ROTOR_INPUT ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on AL_ROTOR_INPUT')
   endif
   if ( .not. has_OMEGA_INPUT ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on OMEGA_INPUT')
   endif
   if ( .not. has_BETA_INPUT ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on BETA_INPUT')
   endif
   if ( .not. has_THET_INPUT ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on THET_INPUT')
   endif
   if ( .not. has_THETT ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on THETT')
   endif
end subroutine read_xml_type_rotortype
subroutine init_xml_type_rotortype_array( dvar )
   type(rotortype), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_rotortype_array
subroutine init_xml_type_rotortype(dvar)
   type(rotortype) :: dvar
end subroutine init_xml_type_rotortype
subroutine write_xml_type_rotortype_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(rotortype), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_rotortype( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_rotortype_array

subroutine write_xml_type_rotortype( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(rotortype)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',TRIM(tag), '>'
   call write_to_xml_integer( info, 'NBLADE_INPUT', indent+3, dvar%NBLADE_INPUT)
   call write_to_xml_real_array( info, 'BLADE_SPACING_INPUT', indent+3, dvar%BLADE_SPACING_INPUT)
   call write_to_xml_real( info, 'R_BLADE_BEGIN_INPUT', indent+3, dvar%R_BLADE_BEGIN_INPUT)
   call write_to_xml_real( info, 'R_BLADE_END_INPUT', indent+3, dvar%R_BLADE_END_INPUT)
   call write_to_xml_integer( info, 'N_CONTROL_INPUT', indent+3, dvar%N_CONTROL_INPUT)
   call write_to_xml_real_array( info, 'R_CONTROL_INPUT', indent+3, dvar%R_CONTROL_INPUT)
   call write_to_xml_real_array( info, 'C_BLADE_INPUT', indent+3, dvar%C_BLADE_INPUT)
   call write_to_xml_real( info, 'AL_ROTOR_INPUT', indent+3, dvar%AL_ROTOR_INPUT)
   call write_to_xml_real( info, 'OMEGA_INPUT', indent+3, dvar%OMEGA_INPUT)
   call write_to_xml_real_array( info, 'BETA_INPUT', indent+3, dvar%BETA_INPUT)
   call write_to_xml_real_array( info, 'THET_INPUT', indent+3, dvar%THET_INPUT)
   call write_to_xml_real( info, 'THETT', indent+3, dvar%THETT)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //TRIM(tag) // '>'
end subroutine write_xml_type_rotortype

subroutine read_xml_file_noise_rotor_parameters(fname, lurep, errout)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep
   logical, intent(out), optional         :: errout

   type(XML_PARSE)                        :: info
   logical                                :: error
   character(len=80)                      :: tag
   character(len=80)                      :: starttag
   logical                                :: endtag
   character(len=80), dimension(1:2,1:20) :: attribs
   integer                                :: noattribs
   character(len=200), dimension(1:100)   :: data
   integer                                :: nodata
   logical                                         :: has_T_VISIT_INPUT
   logical                                         :: has_X_VISIT_INPUT
   logical                                         :: has_V_INF_INPUT
   logical                                         :: has_rotor1
   logical                                         :: has_RHO_INPUT
   logical                                         :: has_VSPEED_INPUT
   has_T_VISIT_INPUT                    = .false.
   has_X_VISIT_INPUT                    = .false.
   has_V_INF_INPUT                      = .false.
   has_rotor1                           = .false.
   has_RHO_INPUT                        = .false.
   has_VSPEED_INPUT                     = .false.

   call init_xml_file_noise_rotor_parameters
   call xml_open( info, fname, .true. )
   call xml_options( info, report_errors=.true., ignore_whitespace=.true.)
   lurep_ = 0
   if ( present(lurep) ) then
      lurep_ = lurep
      call xml_options( info, report_lun=lurep )
   endif
   do
      call xml_get( info, starttag, endtag, attribs, noattribs, &
         data, nodata)
      if ( starttag .ne. '!--' ) exit
   enddo
   if ( starttag .ne. "noise_rotor" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "noise_rotor"')
      error = .true.
      call xml_close(info)
      return
   endif
   strict_ = .true.
   error = .false.
   do
      call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
      if ( xml_error(info) ) then
         write(lurep_,*) 'Error reading input file!'
         error = .true.
         return
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('T_VISIT_INPUT')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            T_VISIT_INPUT, has_T_VISIT_INPUT )
      case('X_VISIT_INPUT')
         call read_xml_real_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            X_VISIT_INPUT, has_X_VISIT_INPUT )
      case('V_INF_INPUT')
         call read_xml_real_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            V_INF_INPUT, has_V_INF_INPUT )
      case('rotor1')
         call read_xml_type_rotortype( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            rotor1, has_rotor1 )
      case('RHO_INPUT')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            RHO_INPUT, has_RHO_INPUT )
      case('VSPEED_INPUT')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            VSPEED_INPUT, has_VSPEED_INPUT )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // TRIM(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_T_VISIT_INPUT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on T_VISIT_INPUT')
   endif
   if ( .not. has_X_VISIT_INPUT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on X_VISIT_INPUT')
   endif
   if ( .not. has_V_INF_INPUT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on V_INF_INPUT')
   endif
   if ( .not. has_rotor1 ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on rotor1')
   endif
   if ( .not. has_RHO_INPUT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on RHO_INPUT')
   endif
   if ( .not. has_VSPEED_INPUT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on VSPEED_INPUT')
   endif
   if ( present(errout) ) errout = error
end subroutine

subroutine write_xml_file_noise_rotor_parameters(fname, lurep)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep

   type(XML_PARSE)                        :: info
   integer                                :: indent = 0

   call xml_open( info, fname, .false. )
   call xml_options( info, report_errors=.true.)
   if ( present(lurep) ) then
       call xml_options( info, report_errors=.true.)
   endif
   write(info%lun,'(a)') &
      '<noise_rotor>'
   call write_to_xml_real( info, 'T_VISIT_INPUT', indent+3, T_VISIT_INPUT)
   call write_to_xml_real_array( info, 'X_VISIT_INPUT', indent+3, X_VISIT_INPUT)
   call write_to_xml_real_array( info, 'V_INF_INPUT', indent+3, V_INF_INPUT)
   call write_xml_type_rotortype( info, 'rotor1', indent+3, rotor1)
   call write_to_xml_real( info, 'RHO_INPUT', indent+3, RHO_INPUT)
   call write_to_xml_real( info, 'VSPEED_INPUT', indent+3, VSPEED_INPUT)
   write(info%lun,'(a)') '</noise_rotor>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_noise_rotor_parameters

end subroutine

end module
