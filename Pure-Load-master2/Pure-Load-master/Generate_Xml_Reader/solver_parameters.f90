module xml_data_solver_parameters
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_
   integer                                         :: LMAX
   integer                                         :: KMAX
   integer                                         :: NTURNS
   integer                                         :: N
   integer                                         :: NC
   integer                                         :: ITMAX
   integer                                         :: NNEAR
   integer                                         :: NTURN_BOUND
   integer                                         :: Ntest
   integer                                         :: IP_WAKE_TYPE
   integer                                         :: IP_UNSTEADY_MODEL
   integer                                         :: IP_CONTINUE_ITER
   integer                                         :: IP_WAKE
   integer                                         :: IFWRD
   integer                                         :: ITRMAX
   integer                                         :: NRPT
   integer                                         :: NPRD
   integer                                         :: NSEG
   real(kind=kind(1.0d0))                          :: EPSD
   real(kind=kind(1.0d0))                          :: EPSV
   real(kind=kind(1.0d0))                          :: EPSA
   real(kind=kind(1.0d0))                          :: BN1
   real(kind=kind(1.0d0))                          :: BN2
   real(kind=kind(1.0d0))                          :: IPRINT
   integer                                         :: IFLOW
   integer                                         :: indu_dim_less
   real(kind=kind(1.0d0)), dimension(:), pointer   :: RAIR => null()
   real(kind=kind(1.0d0)), dimension(:), pointer   :: RMOM => null()
   integer                                         :: NF
   integer                                         :: NMOD
   real(kind=kind(1.0d0))                          :: SHIFT
   real(kind=kind(1.0d0))                          :: SCAL
contains
subroutine read_xml_file_solver_parameters(fname, lurep, errout)
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
   logical                                         :: has_LMAX
   logical                                         :: has_KMAX
   logical                                         :: has_NTURNS
   logical                                         :: has_N
   logical                                         :: has_NC
   logical                                         :: has_ITMAX
   logical                                         :: has_NNEAR
   logical                                         :: has_NTURN_BOUND
   logical                                         :: has_Ntest
   logical                                         :: has_IP_WAKE_TYPE
   logical                                         :: has_IP_UNSTEADY_MODEL
   logical                                         :: has_IP_CONTINUE_ITER
   logical                                         :: has_IP_WAKE
   logical                                         :: has_IFWRD
   logical                                         :: has_ITRMAX
   logical                                         :: has_NRPT
   logical                                         :: has_NPRD
   logical                                         :: has_NSEG
   logical                                         :: has_EPSD
   logical                                         :: has_EPSV
   logical                                         :: has_EPSA
   logical                                         :: has_BN1
   logical                                         :: has_BN2
   logical                                         :: has_IPRINT
   logical                                         :: has_IFLOW
   logical                                         :: has_indu_dim_less
   logical                                         :: has_RAIR
   logical                                         :: has_RMOM
   logical                                         :: has_NF
   logical                                         :: has_NMOD
   logical                                         :: has_SHIFT
   logical                                         :: has_SCAL
   has_LMAX                             = .false.
   has_KMAX                             = .false.
   has_NTURNS                           = .false.
   has_N                                = .false.
   has_NC                               = .false.
   has_ITMAX                            = .false.
   has_NNEAR                            = .false.
   has_NTURN_BOUND                      = .false.
   has_Ntest                            = .false.
   has_IP_WAKE_TYPE                     = .false.
   has_IP_UNSTEADY_MODEL                = .false.
   has_IP_CONTINUE_ITER                 = .false.
   has_IP_WAKE                          = .false.
   has_IFWRD                            = .false.
   has_ITRMAX                           = .false.
   has_NRPT                             = .false.
   has_NPRD                             = .false.
   has_NSEG                             = .false.
   has_EPSD                             = .false.
   has_EPSV                             = .false.
   has_EPSA                             = .false.
   has_BN1                              = .false.
   has_BN2                              = .false.
   has_IPRINT                           = .false.
   has_IFLOW                            = .false.
   has_indu_dim_less                    = .false.
   has_RAIR                             = .false.
   has_RMOM                             = .false.
   has_NF                               = .false.
   has_NMOD                             = .false.
   has_SHIFT                            = .false.
   has_SCAL                             = .false.

   call init_xml_file_solver_parameters
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
   if ( starttag .ne. "solver" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "solver"')
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
      case('LMAX')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            LMAX, has_LMAX )
      case('KMAX')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            KMAX, has_KMAX )
      case('NTURNS')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NTURNS, has_NTURNS )
      case('N')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            N, has_N )
      case('NC')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NC, has_NC )
      case('ITMAX')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            ITMAX, has_ITMAX )
      case('NNEAR')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NNEAR, has_NNEAR )
      case('NTURN_BOUND')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NTURN_BOUND, has_NTURN_BOUND )
      case('Ntest')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            Ntest, has_Ntest )
      case('IP_WAKE_TYPE')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            IP_WAKE_TYPE, has_IP_WAKE_TYPE )
      case('IP_UNSTEADY_MODEL')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            IP_UNSTEADY_MODEL, has_IP_UNSTEADY_MODEL )
      case('IP_CONTINUE_ITER')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            IP_CONTINUE_ITER, has_IP_CONTINUE_ITER )
      case('IP_WAKE')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            IP_WAKE, has_IP_WAKE )
      case('IFWRD')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            IFWRD, has_IFWRD )
      case('ITRMAX')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            ITRMAX, has_ITRMAX )
      case('NRPT')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NRPT, has_NRPT )
      case('NPRD')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NPRD, has_NPRD )
      case('NSEG')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NSEG, has_NSEG )
      case('EPSD')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            EPSD, has_EPSD )
      case('EPSV')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            EPSV, has_EPSV )
      case('EPSA')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            EPSA, has_EPSA )
      case('BN1')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            BN1, has_BN1 )
      case('BN2')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            BN2, has_BN2 )
      case('IPRINT')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            IPRINT, has_IPRINT )
      case('IFLOW')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            IFLOW, has_IFLOW )
      case('indu_dim_less')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            indu_dim_less, has_indu_dim_less )
      case('RAIR')
         call read_xml_double_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            RAIR, has_RAIR )
      case('RMOM')
         call read_xml_double_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            RMOM, has_RMOM )
      case('NF')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NF, has_NF )
      case('NMOD')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            NMOD, has_NMOD )
      case('SHIFT')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            SHIFT, has_SHIFT )
      case('SCAL')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            SCAL, has_SCAL )
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
   if ( .not. has_LMAX ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on LMAX')
   endif
   if ( .not. has_KMAX ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on KMAX')
   endif
   if ( .not. has_NTURNS ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NTURNS')
   endif
   if ( .not. has_N ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on N')
   endif
   if ( .not. has_NC ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NC')
   endif
   if ( .not. has_ITMAX ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on ITMAX')
   endif
   if ( .not. has_NNEAR ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NNEAR')
   endif
   if ( .not. has_NTURN_BOUND ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NTURN_BOUND')
   endif
   if ( .not. has_ITRMAX ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on ITRMAX')
   endif
   if ( .not. has_NRPT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NRPT')
   endif
   if ( .not. has_NPRD ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NPRD')
   endif
   if ( .not. has_NSEG ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NSEG')
   endif
   if ( .not. has_EPSD ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on EPSD')
   endif
   if ( .not. has_EPSV ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on EPSV')
   endif
   if ( .not. has_EPSA ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on EPSA')
   endif
   if ( .not. has_BN1 ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on BN1')
   endif
   if ( .not. has_BN2 ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on BN2')
   endif
   if ( .not. has_IPRINT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on IPRINT')
   endif
   if ( .not. has_IFLOW ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on IFLOW')
   endif
   if ( .not. has_indu_dim_less ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on indu_dim_less')
   endif
   if ( .not. has_RAIR ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on RAIR')
   endif
   if ( .not. has_RMOM ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on RMOM')
   endif
   if ( .not. has_NF ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NF')
   endif
   if ( .not. has_NMOD ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on NMOD')
   endif
   if ( .not. has_SHIFT ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on SHIFT')
   endif
   if ( .not. has_SCAL ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on SCAL')
   endif
   if ( present(errout) ) errout = error
end subroutine

subroutine write_xml_file_solver_parameters(fname, lurep)
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
      '<solver>'
   call write_to_xml_integer( info, 'LMAX', indent+3, LMAX)
   call write_to_xml_integer( info, 'KMAX', indent+3, KMAX)
   call write_to_xml_integer( info, 'NTURNS', indent+3, NTURNS)
   call write_to_xml_integer( info, 'N', indent+3, N)
   call write_to_xml_integer( info, 'NC', indent+3, NC)
   call write_to_xml_integer( info, 'ITMAX', indent+3, ITMAX)
   call write_to_xml_integer( info, 'NNEAR', indent+3, NNEAR)
   call write_to_xml_integer( info, 'NTURN_BOUND', indent+3, NTURN_BOUND)
   call write_to_xml_integer( info, 'Ntest', indent+3, Ntest)
   call write_to_xml_integer( info, 'IP_WAKE_TYPE', indent+3, IP_WAKE_TYPE)
   call write_to_xml_integer( info, 'IP_UNSTEADY_MODEL', indent+3, IP_UNSTEADY_MODEL)
   call write_to_xml_integer( info, 'IP_CONTINUE_ITER', indent+3, IP_CONTINUE_ITER)
   call write_to_xml_integer( info, 'IP_WAKE', indent+3, IP_WAKE)
   call write_to_xml_integer( info, 'IFWRD', indent+3, IFWRD)
   call write_to_xml_integer( info, 'ITRMAX', indent+3, ITRMAX)
   call write_to_xml_integer( info, 'NRPT', indent+3, NRPT)
   call write_to_xml_integer( info, 'NPRD', indent+3, NPRD)
   call write_to_xml_integer( info, 'NSEG', indent+3, NSEG)
   call write_to_xml_double( info, 'EPSD', indent+3, EPSD)
   call write_to_xml_double( info, 'EPSV', indent+3, EPSV)
   call write_to_xml_double( info, 'EPSA', indent+3, EPSA)
   call write_to_xml_double( info, 'BN1', indent+3, BN1)
   call write_to_xml_double( info, 'BN2', indent+3, BN2)
   call write_to_xml_double( info, 'IPRINT', indent+3, IPRINT)
   call write_to_xml_integer( info, 'IFLOW', indent+3, IFLOW)
   call write_to_xml_integer( info, 'indu_dim_less', indent+3, indu_dim_less)
   call write_to_xml_double_array( info, 'RAIR', indent+3, RAIR)
   call write_to_xml_double_array( info, 'RMOM', indent+3, RMOM)
   call write_to_xml_integer( info, 'NF', indent+3, NF)
   call write_to_xml_integer( info, 'NMOD', indent+3, NMOD)
   call write_to_xml_double( info, 'SHIFT', indent+3, SHIFT)
   call write_to_xml_double( info, 'SCAL', indent+3, SCAL)
   write(info%lun,'(a)') '</solver>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_solver_parameters
   Ntest = 24
   IP_WAKE_TYPE = 0
   IP_UNSTEADY_MODEL = 0
   IP_CONTINUE_ITER = 0
   IP_WAKE = 0
   IFWRD = 0

end subroutine

end module
