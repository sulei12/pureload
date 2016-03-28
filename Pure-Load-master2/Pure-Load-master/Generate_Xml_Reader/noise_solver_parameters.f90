module xml_data_noise_solver_parameters
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_
   integer                                         :: N_TIME_INPUT
   integer                                         :: N_R_INPUT
   integer                                         :: EQRGRID
   real                                            :: R_PARA1
   real                                            :: R_PARA2
   integer                                         :: N_C_INPUT
   integer                                         :: EQCGRID
   real                                            :: C_PARA1
   real                                            :: C_PARA2
   integer                                         :: NTIME_INPUT
   integer                                         :: NPEROID_INPUT
contains
subroutine read_xml_file_noise_solver_parameters(fname, lurep, errout)
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
   logical                                         :: has_N_TIME_INPUT
   logical                                         :: has_N_R_INPUT
   logical                                         :: has_EQRGRID
   logical                                         :: has_R_PARA1
   logical                                         :: has_R_PARA2
   logical                                         :: has_N_C_INPUT
   logical                                         :: has_EQCGRID
   logical                                         :: has_C_PARA1
   logical                                         :: has_C_PARA2
   logical                                         :: has_NTIME_INPUT
   logical                                         :: has_NPEROID_INPUT
   has_N_TIME_INPUT                     = .false.
   has_N_R_INPUT                        = .false.
   has_EQRGRID                          = .false.
   has_R_PARA1                          = .false.
   has_R_PARA2                          = .false.
   has_N_C_INPUT                        = .false.
   has_EQCGRID                          = .false.
   has_C_PARA1                          = .false.
   has_C_PARA2                          = .false.
   has_NTIME_INPUT                      = .false.
   has_NPEROID_INPUT                    = .false.

   call init_xml_file_noise_solver_parameters
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
   if ( starttag .ne. "noise_solver_parameters" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "noise_solver_parameters"')
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
      case('N_TIME_INPUT')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            N_TIME_INPUT, has_N_TIME_INPUT )
      case('N_R_INPUT')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            N_R_INPUT, has_N_R_INPUT )
      case('EQRGRID')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            EQRGRID, has_EQRGRID )
      case('R_PARA1')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            R_PARA1, has_R_PARA1 )
      case('R_PARA2')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            R_PARA2, has_R_PARA2 )
      case('N_C_INPUT')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            N_C_INPUT, has_N_C_INPUT )
      case('EQCGRID')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            EQCGRID, has_EQCGRID )
      case('C_PARA1')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            C_PARA1, has_C_PARA1 )
      case('C_PARA2')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            C_PARA2, has_C_PARA2 )
      case('NTIME_INPUT')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NTIME_INPUT, has_NTIME_INPUT )
      case('NPEROID_INPUT')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NPEROID_INPUT, has_NPEROID_INPUT )
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
   if ( .not. has_N_TIME_INPUT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on N_TIME_INPUT')
   endif
   if ( .not. has_N_R_INPUT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on N_R_INPUT')
   endif
   if ( .not. has_N_C_INPUT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on N_C_INPUT')
   endif
   if ( .not. has_NTIME_INPUT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NTIME_INPUT')
   endif
   if ( .not. has_NPEROID_INPUT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NPEROID_INPUT')
   endif
   if ( present(errout) ) errout = error
end subroutine

subroutine write_xml_file_noise_solver_parameters(fname, lurep)
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
      '<noise_solver_parameters>'
   call write_to_xml_integer( info, 'N_TIME_INPUT', indent+3, N_TIME_INPUT)
   call write_to_xml_integer( info, 'N_R_INPUT', indent+3, N_R_INPUT)
   call write_to_xml_integer( info, 'EQRGRID', indent+3, EQRGRID)
   call write_to_xml_real( info, 'R_PARA1', indent+3, R_PARA1)
   call write_to_xml_real( info, 'R_PARA2', indent+3, R_PARA2)
   call write_to_xml_integer( info, 'N_C_INPUT', indent+3, N_C_INPUT)
   call write_to_xml_integer( info, 'EQCGRID', indent+3, EQCGRID)
   call write_to_xml_real( info, 'C_PARA1', indent+3, C_PARA1)
   call write_to_xml_real( info, 'C_PARA2', indent+3, C_PARA2)
   call write_to_xml_integer( info, 'NTIME_INPUT', indent+3, NTIME_INPUT)
   call write_to_xml_integer( info, 'NPEROID_INPUT', indent+3, NPEROID_INPUT)
   write(info%lun,'(a)') '</noise_solver_parameters>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_noise_solver_parameters
   EQRGRID = 1
   R_PARA1 = 0
   R_PARA2 = 0
   EQCGRID = 1
   C_PARA1 = 0
   C_PARA2 = 0

end subroutine

end module
