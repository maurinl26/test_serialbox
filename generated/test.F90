#define ACC_PREFIX !$acc
program testSerialBox

#ifdef SERIALIZE
USE m_serialize, ONLY: &
  fs_create_savepoint, &
  fs_add_savepoint_metainfo, &
  fs_read_field, &
  fs_write_field
USE utils_ppser, ONLY:  &
  ppser_finalize, &
  ppser_set_mode, &
  ppser_initialize, &
  ppser_get_mode, &
  ppser_savepoint, &
  ppser_serializer, &
  ppser_serializer_ref, &
  ppser_intlength, &
  ppser_reallength, &
  ppser_realtype, &
  ppser_zrperturb
#endif


    implicit none
  
    real, dimension(:,:,:), allocatable :: Qin_out, MASS
    real, dimension(:,:),   allocatable :: FILLQ_out
  
    integer :: N = 5
  
    allocate(Qin_out(N,N,N), MASS(N,N,N), FILLQ_out(N,N))
  
    call random_number(Qin_out)
    call random_number(MASS)
  
    where(Qin_out < 0.1) Qin_out = -Qin_out
  
    print*, 'sum(Qin_out) = ', sum(Qin_out)
    print*, 'sum(MASS) = ', sum(MASS)
  
  
#ifdef SERIALIZE
PRINT *, '>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<'
PRINT *, '>>> WARNING: SERIALIZATION IS ON <<<'
PRINT *, '>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<'

! setup serialization environment
call ppser_initialize( &
           directory='.', &
           prefix='FILLQ2ZERO_InOut')
call fs_create_savepoint('sp1', ppser_savepoint)
call ppser_set_mode(0)
! file: /home/maurinl/maurinl26/test_serialbox/test.F90 lineno: #24
SELECT CASE ( ppser_get_mode() )
  CASE(0)
    call fs_write_field(ppser_serializer, ppser_savepoint, 'q_in', Qin_out)
    call fs_write_field(ppser_serializer, ppser_savepoint, 'm_in', MASS)
    call fs_write_field(ppser_serializer, ppser_savepoint, 'fq_in', FILLQ_out)
  CASE(1)
    call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'q_in', Qin_out)
    call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'm_in', MASS)
    call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'fq_in', FILLQ_out)
  CASE(2)
    call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'q_in', Qin_out, ppser_zrperturb)
    call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'm_in', MASS, ppser_zrperturb)
    call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'fq_in', FILLQ_out, ppser_zrperturb)
END SELECT
#endif
  
    call FILLQ2ZERO1(Qin_out, MASS, FILLQ_out)
  
#ifdef SERIALIZE
! file: /home/maurinl/maurinl26/test_serialbox/test.F90 lineno: #28
SELECT CASE ( ppser_get_mode() )
  CASE(0)
    call fs_write_field(ppser_serializer, ppser_savepoint, 'q_out', Qin_out)
    call fs_write_field(ppser_serializer, ppser_savepoint, 'm_out', MASS)
    call fs_write_field(ppser_serializer, ppser_savepoint, 'fq_out', FILLQ_out)
  CASE(1)
    call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'q_out', Qin_out)
    call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'm_out', MASS)
    call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'fq_out', FILLQ_out)
  CASE(2)
    call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'q_out', Qin_out, ppser_zrperturb)
    call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'm_out', MASS, ppser_zrperturb)
    call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'fq_out', FILLQ_out, ppser_zrperturb)
END SELECT
! cleanup serialization environment
call ppser_finalize()
#endif
    print*, 'sum(Qin_out) = ', sum(Qin_out)
    print*, 'sum(FILLQ_out) = ', sum(FILLQ_out)
  
     contains
  
    subroutine FILLQ2ZERO1( Q, MASS, FILLQ  )

#ifdef SERIALIZE
USE m_serialize, ONLY: &
  fs_create_savepoint, &
  fs_add_savepoint_metainfo, &
  fs_read_field, &
  fs_write_field
USE utils_ppser, ONLY:  &
  ppser_finalize, &
  ppser_set_mode, &
  ppser_initialize, &
  ppser_get_mode, &
  ppser_savepoint, &
  ppser_serializer, &
  ppser_serializer_ref, &
  ppser_intlength, &
  ppser_reallength, &
  ppser_realtype, &
  ppser_zrperturb
#endif

      real, dimension(:,:,:),   intent(inout)  :: Q
#ifdef SERIALIZE
      real, dimension(:,:,:)     :: MASS
#else
      real, dimension(:,:,:),   intent(in)     :: MASS
#endif
      real, dimension(:,:),     intent(  out)  :: FILLQ
      integer                                  :: IM,JM,LM
      integer                                  :: I,J,K,L
      real                                     :: TPW, NEGTPW
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Fills in negative q values in a mass conserving way.
      ! Conservation of TPW was checked.
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IM = SIZE( Q, 1 )
      JM = SIZE( Q, 2 )
      LM = SIZE( Q, 3 )
      do j=1,JM
         do i=1,IM
            TPW = SUM( Q(i,j,:)*MASS(i,j,:) )
            NEGTPW = 0.
            do l=1,LM
               if ( Q(i,j,l) < 0.0 ) then
                  NEGTPW   = NEGTPW + ( Q(i,j,l)*MASS( i,j,l ) )
                  Q(i,j,l) = 0.0
               endif
            enddo
            do l=1,LM
               if ( Q(i,j,l) >= 0.0 ) then
                  Q(i,j,l) = Q(i,j,l)*( 1.0+NEGTPW/(TPW-NEGTPW) )
               endif
            enddo
            FILLQ(i,j) = -NEGTPW
         end do
      end do
    end subroutine FILLQ2ZERO1
  end program