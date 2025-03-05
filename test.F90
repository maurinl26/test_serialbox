program testSerialBox

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
  
  
  !$ser init directory='.' prefix='FILLQ2ZERO_InOut'
  !$ser savepoint sp1
  !$ser mode write
  !$ser data q_in=Qin_out m_in=MASS fq_in=FILLQ_out
  
    call FILLQ2ZERO1(Qin_out, MASS, FILLQ_out)
  
  !$ser data q_out=Qin_out m_out=MASS fq_out=FILLQ_out
  !$ser cleanup
    print*, 'sum(Qin_out) = ', sum(Qin_out)
    print*, 'sum(FILLQ_out) = ', sum(FILLQ_out)
  
     contains
  
    subroutine FILLQ2ZERO1( Q, MASS, FILLQ  )
      real, dimension(:,:,:),   intent(inout)  :: Q
      real, dimension(:,:,:),   intent(in)     :: MASS
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