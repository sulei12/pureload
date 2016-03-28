module xml_data_noise_input_parameters
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_
   character(len=80)                                :: noise_file_name
   character(len=80)                                :: noise_solver_file_name
   character(len=80)                                :: noise_rotor_file_name
contains
subroutine read_xml_file_noise_input_parameters(fname, lurep, errout)
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
   logical                                         :: has_noise_file_name
   logical                                         :: has_noise_solver_file_name
   logical                                         :: has_noise_rotor_file_name
   has_noise_file_name                  = .false.
   has_noise_solver_file_name           = .false.
   has_noise_rotor_file_name            = .false.

   call init_xml_file_noise_input_parameters
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
   if ( starttag .ne. "noise_input_parameters" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "noise_input_parameters"')
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
      case('noise_file_name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            noise_file_name, has_noise_file_name )
      case('noise_solver_file_name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            noise_solver_file_name, has_noise_solver_file_name )
      case('noise_rotor_file_name')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            noise_rotor_file_name, has_noise_rotor_file_name )
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
   if ( .not. has_noise_file_name ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on noise_file_name')
   endif
   if ( .not. has_noise_solver_file_name ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on noise_solver_file_name')
   endif
   if ( .not. has_noise_rotor_file_name ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on noise_rotor_file_name')
   endif
   if ( present(errout) ) errout = error
end subroutine

subroutine write_xml_file_noise_input_parameters(fname, lurep)
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
      '<noise_input_parameters>'
   call write_to_xml_word( info, 'noise_file_name', indent+3, noise_file_name)
   call write_to_xml_word( info, 'noise_solver_file_name', indent+3, noise_solver_file_name)
   call write_to_xml_word( info, 'noise_rotor_file_name', indent+3, noise_rotor_file_name)
   write(info%lun,'(a)') '</noise_input_parameters>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_noise_input_parameters

end subroutine

end module
