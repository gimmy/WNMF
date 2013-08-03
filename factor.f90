SUBROUTINE factor (A, W, row, col, rank, U, V)
  ! Fattorizzazione NonNegativa pesata di A
  ! Fattorizza A in UV con matrice dei pesi W

  ! A*B is the Hadamard product
  ! A/B is the Hadamard division

  INTEGER row, col, rank
  INTEGER (kind = 4), DIMENSION(row,col) :: A
  REAL, DIMENSION(row,col) :: W, UV
  REAL, DIMENSION(row,rank) :: U, U_aux, U_aux2
  REAL, DIMENSION(rank,col) :: V

  INTEGER i, j, steps
  ! INTEGER, parameter :: sigma = 30
  REAL, parameter :: eps = 1e-5, precision = 1e4
  REAL (kind = 4) KLdivergence, KL, Euclidea, euclid

  REAL, DIMENSION(row,col) :: E, AUX
  LOGICAL HELP

  ! Creo la matrice E per evitare la divisione per zero
  DO i = 1, row
     DO j = 1, col
        E(i,j) = eps
     END DO
  END DO

  ! Tiro a caso U e calcolo V
  CALL RANDOM_SEED
  CALL RANDOM_NUMBER(U)

  CALL createV(A,U,V, row, rank, col)

  steps = 1
  ! WRITE(*,*) 'Shape di U:', SHAPE(U), 'Shape di V:', SHAPE(V)
  WRITE (*,*) 'Inizio fattorizzazione...'
  DO
     UV = MATMUL(U,V)
     WRITE(*,*) 'Calcolata UV'

     HELP = .FALSE.
     ! Check Zero in UV
     DO i = 1, row
        DO j = 1, col
           IF ( UV(i,j) == 0 ) THEN
              HELP = .TRUE.
           ENDIF
        END DO
     END DO
     IF (HELP) THEN
        STOP "Zero in UV!"
     ELSE
        WRITE(*,*) 'Nessuno zero in UV, continuo..'
     ENDIF

     ! KL = KLdivergence(A, UV, W, row, col, eps)
     ! WRITE(*,*) 'KL: ', KL
     ! IF ( KL < precision ) exit

     ! Aggiorno matrice V
     ! V = ( V/MATMUL(transpose(U),W) ) * ( MATMUL(transpose(U),(W*A)/(UV + E)) )

     ! Aggiorno U
     ! U = ( U/MATMUL(W,transpose(V)) ) * ( MATMUL((W*A)/(UV + E),transpose(V)) )

     U_aux = U/MATMUL( W,transpose(V) )

     ! Check Non NaN in U_aux
     DO i = 1, row
        DO j = 1, rank
           IF (isnan( U_aux(i,j) )) THEN
              ! U(i,j) = 0
              WRITE (*,*) "NaN in U_aux"
              exit
           ENDIF
           ! IF (isinf( U_aux(i,j) )) THEN
           !    STOP "Inf in U_aux"
           ! ENDIF
        END DO
     END DO

     AUX = (W*A)/(UV + E)

     ! Check Non NaN in U
     DO i = 1, row
        DO j = 1, col
           IF (isnan( AUX(i,j) )) THEN
              ! U(i,j) = 0
              WRITE (*,*) "NaN in AUX"
              exit
           ENDIF
        END DO
     END DO

     write (*,*) 'sono vivo. calcolata AUX'
     U_aux2 = MATMUL(AUX,transpose(V))

     ! Check Non NaN in U_aux2
     DO i = 1, row
        DO j = 1, rank
           IF (isnan( U_aux2(i,j) )) THEN
              ! U(i,j) = 0
              WRITE (*,*) "NaN in U_aux2"
              exit
           ENDIF
        END DO
     END DO

     write (*,*) 'sono vivo, calcolata U_aux2'

     U = U_aux * U_aux2

     write(*,*) 'U_aux(1,1): ', U_aux(1,1),' U_aux2(1,1):', U_aux2(1,1)

     ! Check Non NaN in U
     DO i = 1, row
        DO j = 1, rank
           IF (isnan( U(i,j) )) THEN
              write(*,'(A,I3,A,I3)') 'i:',i,' j:',j
              STOP "NaN in U in "
           ENDIF
        END DO
     END DO


     WRITE(*,*) 'Step: ', steps

     UV = MATMUL(U,V)
     WRITE(*,*) 'Ricalcolata UV'

     HELP = .FALSE.
     ! Check Zero in UV
     DO i = 1, row
        DO j = 1, col
           IF ( UV(i,j) == 0 ) THEN
              HELP = .TRUE.
           ENDIF
        END DO
     END DO
     IF (HELP) THEN
        STOP "Zero in UV!"
     ENDIF

     ! A = REAL(A)
     KL = KLdivergence(A, UV, W, row, col, eps)
     WRITE(*,*) 'KL: ', KL

     IF ( KL < precision ) exit
     WRITE (*,*) 'KL Div [U update]: ', KL


     ! V1 = MATMUL( transpose(U),(W*A)/(UV + E) )
     ! V1 = ( V/MATMUL(transpose(U),W) ) * V1

     ! U = ( U/MATMUL(W,transpose(V)) ) * ( MATMUL((W*A)/(UV + E),transpose(V)) )
     ! Va = ( V/MATMUL(transpose(U),W) ) * ( MATMUL(transpose(U),(W*A)/(UV + E)) )
     ! V = Va


     ! Aggiorno matrice V
     ! V = ( V/MATMUL(transpose(U),W) ) * ( MATMUL(transpose(U),(W*A)/(UV + E)) )
     ! KLDiv = KLdivergence(A, UV, W, row, col, eps)
     ! IF ( KLDiv < precision ) exit
     ! WRITE (*,*) 'KL Div [V update]: ', KLDiv

     ! Aggiorno matrice U

     ! U_aux = MATMUL( W,transpose(V) )
     ! AUX = (W*A)/(UV + E)
     ! write (*,*) 'sono vivo. calcolata AUX'
     ! U_aux2 = MATMUL(AUX,transpose(V))
     ! write (*,*) 'sono vivo, calcolata U_aux2'
     ! U = U_aux * U_aux2

     ! euclid = Euclidea(A, UV, row, col)
     ! IF ( euclid < precision ) exit
     steps = steps+1     
  END DO

  WRITE (*,*) ''
  WRITE (*,*) 'Precision: ', precision 
  WRITE (*,*) 'KL Divergence: ', KL
  WRITE(*,*) 'Passi: ', steps

END SUBROUTINE factor

function KLdivergence (A, UV, W, row, col, eps) result(KLDiv)
  ! Calcola la divergenza di Kullback-Leibler pesata
  ! IMPLICIT NONE
  INTEGER row, col
  INTEGER, DIMENSION (row,col) :: A
  REAL, DIMENSION (row,col) :: UV, W, DIV
  INTEGER i, j
  REAL KLDiv, eps

  WRITE(*,*) 'Entro in KL function'

  DO i = 1, row                 
     DO j = 1, col
        IF (isnan( REAL(A(i,j)) )) THEN
           WRITE(*,'(A,I4,A,I3,A,F5.3)') 'i:',i,' j:',j,'  DIV is ', DIV(i,j)
           STOP '"A is NaN"'
        END IF
     END DO
  END DO

  ! write ( *, '(a)' ) '  Sample data of A real:'
  ! write ( *, '(a)' ) ' '
  ! do k = 1, 10
  !    ! i = ( ( 10 - k ) * 1 + ( k - 1 ) * nrow ) / ( 10 - 1 )
  !    ! j = ( ( 10 - k ) * 1 + ( k - 1 ) * ncol ) / ( 10 - 1 )
  !    write ( *, * ) k, A(k+10,1)
  ! end do

  WRITE(*,*) ''
  WRITE(*,'(A,I4,A,I3,A,I5)') 'i:',1,' j:',1,'  A(1,1): ', A(1,1)
  WRITE(*,'(A,I4,A,I3,A,F5.3)') 'i:',1,' j:',1,'  UV(1,1): ', UV(1,1)

  KLDiv = 0
  DO i = 1, row                 
     DO j = 1, col
        DIV(i,j) = log( A(i,j)/UV(i,j) + eps)
        IF (isnan( DIV(i,j) )) THEN
           WRITE(*,*) 'i:',i,' j:',j
           STOP '"DIV is NaN [in log]"'
        END IF
     END DO
  END DO

  DO i = 1, row                 
     DO j = 1, col
        IF (isnan( UV(i,j) )) THEN
           WRITE(*,'(A,I4,A,I3,A,F5.3)') 'i:',i,' j:',j
           STOP '"UV is NaN"'
        END IF
        IF (isnan( W(i,j) )) THEN
           WRITE(*,'(A,I4,A,I3,A,F5.3)') 'i:',i,' j:',j
           STOP '"W is NaN"'
        END IF
     END DO
  END DO


  DIV = A * DIV - A + UV

  DO i = 1, row
     DO j = 1, col
        IF (isnan( DIV(i,j) )) THEN
           WRITE(*,'(A,I4,A,I3,A,F5.3)') 'i:',i,' j:',j,'  DIV is ', DIV(i,j)
           STOP '"DIV is NaN [in A*DIV]"'
        END IF
     END DO
  END DO

  WRITE(*,*) ' '
  WRITE(*,*) 'Prima: '
  WRITE(*,'(A,I4,A,I3,A,F5.3)') 'i:',1,' j:',1,'  DIV is ', DIV(1,1)
  WRITE(*,'(A,I4,A,I3,A,F5.3)') 'i:',1,' j:',1,'  W is ', W(1,1)

  ! DIV = W * ( A * DIV - A + UV )
  DIV = W * ( DIV )

  WRITE(*,*) 'Dopo'
  WRITE(*,'(A,I4,A,I3,A,F5.3)') 'i:',1,' j:',1,'  W * DIV is ', DIV(1,1)

  DO i = 1, row
     DO j = 1, col
        IF (isnan( DIV(i,j) )) THEN
           WRITE(*,'(A,I4,A,I3,A,F5.3)') 'i:',i,' j:',j,'  DIV is ', DIV(i,j)
           STOP '"DIV is NaN"'
        END IF
     END DO
  END DO


  DO i = 1, row
     DO j = 1, col
        KLDiv = KLDiv + DIV(i,j)
        IF (isnan( KLDiv )) THEN
           WRITE(*,'(A,I4,A,I3,A,F5.3)') 'i:',i,' j:',j,'  KLDiv is ',KLDiv
           STOP '"KLDiv is NaN"'
        END IF
        IF ( KLDiv < 0 ) THEN
           WRITE (*,*) 'KL sotto zero: ', KLDiv
        END IF
     END DO
  END DO

  ! WRITE (*,*) 'KL Divergence: ', KLDiv

END function KLdivergence

function Euclidea(A, UV, row, col) result(euclid)
  ! Calcola la norma euclidea pesata
  IMPLICIT NONE
  INTEGER row, col
  INTEGER, DIMENSION (row,col) :: A
  REAL, DIMENSION (row,col) :: UV, W, AUX
  INTEGER i, j
  REAL euclid
  
  euclid = 0
  AUX = 0.5 * W * (A-UV) * (A-UV)

  DO i = 1, row
     DO j = 1, col
        euclid = euclid + AUX(i,j)
        IF ( euclid < 0 ) THEN
           WRITE (*,*) 'distanza sotto zero: ', euclid
           exit
        END IF
     END DO
  END DO
  WRITE (*,*) 'Euclidean Distance =', euclid

END function Euclidea
