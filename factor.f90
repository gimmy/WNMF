SUBROUTINE factor (A, W, row, col, rank, UV, maxUV, maxiter)
  ! Fattorizzazione NonNegativa pesata di A
  ! Fattorizza A in UV con matrice dei pesi W

  ! A*B is the Hadamard product
  ! A/B is the Hadamard division

  INTEGER row, col, rank
  INTEGER (kind = 4), DIMENSION(row,col) :: A
  REAL, DIMENSION(row,col) :: W, UV
  REAL (kind = 8), DIMENSION(row,rank) :: U
  REAL (kind = 8), DIMENSION(rank,col) :: V

  INTEGER i, j, steps, maxiter
  DOUBLE PRECISION, parameter :: eps = 1e-7, precision = 1e4
  REAL (kind = 8) KLdivergence, KL, Euclidea, euclid

  REAL norm_infty, maxUV, lastmaxUV
  REAL, DIMENSION(row,col) :: E

  ! Creo la matrice E per evitare la divisione per zero
  DO i = 1, row
     DO j = 1, col
        E(i,j) = eps
     END DO
  END DO

  ! Tiro a caso U e calcolo V
  CALL RANDOM_SEED()
  CALL RANDOM_NUMBER(U)
  U = int(U*10)
  CALL RANDOM_NUMBER(V)
  V = int(V*10)
  ! CALL createV(A,U,V, row, rank, col)
  V = int(V)

  UV = MATMUL(U,V)
  WRITE(*,*) 'Calcolata UV'
  call checkUV(UV, row, col, maxUV)

  WRITE (*,*) 'Inizio fattorizzazione...'
  DO steps = 1, maxiter
     ! Aggiorno U
     WRITE (*,'(A,I2,A)', advance='no') '[', steps, ' ] Aggiorno U '
     U = ( U/MATMUL(W,transpose(V)) ) * ( MATMUL((W*A)/(UV + E),transpose(V)) )

     ! ! Check Non NaN in U
     ! DO i = 1, row
     !    DO j = 1, rank
     !       IF (isnan( U(i,j) )) THEN
     !          write(*,'(A,I3,A,I3,A,I3)') 'i:',i,' j:',j, 'step:', steps
     !          STOP "NaN in U"
     !       ENDIF
     !    END DO
     ! END DO

     UV = MATMUL(U,V)
     KL = KLdivergence(A, UV, W, row, col, eps)
     WRITE (*,'(A,F11.2,A)', advance='no') ' (KL Div: ', KL, ')'
     call checkUV(UV, row, col, maxUV)
	lastmaxUV = maxUV
     ! IF ( steps == maxiter ) THEN
     !    UV = MATMUL(U,V)
     !    KL = KLdivergence(A, UV, W, row, col, eps)
     !    WRITE (*,*) 'KL Div [U update]: ', KL
     ! ENDIF


     ! ! Ricalcolo V
    !  IF (( MOD(steps,2) == 0 ) .and. (steps < 10)) THEN
      !  write(*,*) ''
       ! CALL createV(A,U,V, row, rank, col)
     ! endif

     ! Aggiorno matrice V
     ! WRITE (*,'(A,I2,A)', advance='no') '[', steps, ' ] Aggiorno V '
     WRITE (*,'(A)', advance='no') ' | Aggiorno V '
     V = ( V/MATMUL(transpose(U),W) ) * ( MATMUL(transpose(U),(W*A)/(UV + E)) )
     ! Vaux = ( V/MATMUL(transpose(U),W) ) * ( MATMUL(transpose(U),(W*A)/(UV + E)) )

     ! V = Vaux
     UV = MATMUL(U,V)
     call checkUV(UV, row, col, maxUV)
!     IF ( ( lastmaxUV + 6 ) < maxUV ) THEN
!        write(*,*), ''
!        write(*,*) 'Riparto...'
!        CALL createV(A,U,V, row, rank, col)
!     ENDIF
     
     KL = KLdivergence(A, UV, W, row, col, eps)
     WRITE (*,*) ' | KL Div: ', KL
     IF ( (KL < precision) .or. (maxUV <= 255) ) EXIT

     ! IF ( steps == maxiter ) THEN
     !    WRITE (*,*) 'KL Div [UV update]: ', KL
     ! ENDIF

     ! euclid = Euclidea(A, UV, row, col)
  END DO
  
  WRITE(*,*) 'Norma infinito U: ', norm_infty(U, row, rank)
  WRITE(*,*) 'Norma infinito V: ', norm_infty(V, rank, col)

  WRITE(*,*) ''
  WRITE(*,'(A,I2)', advance='no') 'Passi: ', steps
  WRITE(*,'(A,F10.2)', advance='no') ' Precisione: ', precision 
  WRITE(*,*) 'KL Divergence: ', KL

END SUBROUTINE factor

SUBROUTINE checkUV(UV, row, col, maxUV)
  LOGICAL HELP, TOOBIG
  REAL maxUV
  integer row, col, i ,j
  REAL, DIMENSION(row,col) :: UV

  maxUV = 0
  HELP = .FALSE.
  TOOBIG = .FALSE.

  ! Check Zero in UV
  DO i = 1, row
     DO j = 1, col
        IF ( UV(i,j) == 0 ) THEN
           HELP = .TRUE.
        ENDIF
        IF ( UV(i,j) > 255 ) THEN
           TOOBIG = .TRUE.
           IF ( maxUV < UV(i,j) ) then
              maxUV = UV(i,j)
           ENDIF
        ENDIF
     END DO
  END DO
  IF (HELP) THEN
     STOP "Zero in UV!"
  ENDIF
  IF (TOOBIG) THEN
     WRITE(*,'(A,F8.2)', advance='no') " max in UV: ", maxUV
  ELSE
     WRITE(*,'(A,F3.2)', advance='no') "UV nella media, max:", maxUV
  ENDIF

ENDSUBROUTINE checkUV

function KLdivergence (A, UV, W, row, col, eps) result(KLDiv)
  ! Calcola la divergenza di Kullback-Leibler pesata
  ! IMPLICIT NONE
  INTEGER row, col
  INTEGER, DIMENSION (row,col) :: A
  REAL (kind = 8), DIMENSION (row,col) :: UV, W, DIV
  INTEGER i, j
  REAL (kind = 8) KLDiv, eps, normU, normV, norm

  ! norm = normU*normV

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

  DIV = W * ( A * DIV - A + UV )

  ! DO i = 1, row
  !    DO j = 1, col
  !       IF (isnan( DIV(i,j) )) THEN
  !          WRITE(*,'(A,I4,A,I3,A,F5.3)') 'i:',i,' j:',j,'  DIV is ', DIV(i,j)
  !          STOP '"DIV is NaN"'
  !       END IF
  !    END DO
  ! END DO

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

function norm_infty (B, row, col) result(norm)
  ! Calcola la norma infinito di B
  IMPLICIT NONE
  INTEGER row, col
  REAL, DIMENSION (row,col) :: B
  REAL, DIMENSION (col) :: v
  INTEGER i
  REAL aux, norm

  v = B(1,:)
  norm = sum( v )

  DO i = 1, row
     v = B(i,:)
     aux = sum( v )
     IF ( aux > norm ) THEN
        norm = aux
     ENDIF
  END DO

END function norm_infty
