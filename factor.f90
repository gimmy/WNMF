SUBROUTINE factor (A, W, row, col, rank, UV, maxUV, maxiter)
  ! Fattorizzazione NonNegativa pesata di A
  ! Fattorizza A in UV con matrice dei pesi W

  ! A*B is the Hadamard product
  ! A/B is the Hadamard division

  INTEGER row, col, rank
  INTEGER, DIMENSION(row,col) :: A
  REAL(KIND(0.d0)), DIMENSION(row,col) :: W, UV
  REAL(KIND(0.d0)), DIMENSION(row,rank) :: U
  REAL(KIND(0.d0)), DIMENSION(rank,col) :: V

  INTEGER i, j, steps, maxiter
  REAL (KIND(0.d0)), parameter :: eps = 1e-37, precision = 1e2
  REAL (KIND(0.d0)) KLdivergence, KL, Euclidea, euclid

  REAL norm_infty, maxUV, lastmaxUV
  REAL(KIND(0.d0)), DIMENSION(row,col) :: E

  CHARACTER ( len = 80 ) filename

  ! Creo la matrice E per evitare la divisione per zero
  DO i = 1, row
     DO j = 1, col
        E(i,j) = eps
     END DO
  END DO

  ! Tiro a caso U e calcolo V
  CALL RANDOM_SEED()
  CALL RANDOM_NUMBER(U)
  U = int(U*100)
  CALL RANDOM_NUMBER(V)
  V = int(V*100)
  ! CALL createV(A,U,V, row, rank, col)
  ! V = int(V)

  UV = MATMUL(U,V)
  WRITE(*,*) 'Calcolata UV'
  ! call checkUV(UV, row, col, maxUV)

  WRITE (*,*) 'Inizio fattorizzazione...'
  DO steps = 1, maxiter
     ! Aggiorno U
     WRITE (*,'(A,I2,A)', advance='no') '[', steps, ' ] Aggiorno U '
     U = ( U/MATMUL(W,transpose(V)) ) * ( MATMUL((W*A)/(UV + E),transpose(V)) )

     UV = MATMUL(U,V)
     KL = KLdivergence(A, UV, W, row, col, eps)
     WRITE (*,'(A,F11.2,A)', advance='no') ' (KL Div: ', KL, ')'
     call checkUV(UV, row, col, maxUV)
	lastmaxUV = maxUV

     ! ! Ricalcolo V
     ! IF (( MOD(steps,2) == 0 ) .and. (steps < 10)) THEN
     !   write(*,*) ''
     !   CALL createV(A,U,V, row, rank, col)
     ! ENDIF

     ! Aggiorno matrice V
     ! WRITE (*,'(A,I2,A)', advance='no') '[', steps, ' ] Aggiorno V '
     WRITE (*,'(A)', advance='no') ' | Aggiorno V '
     V = ( V/MATMUL(transpose(U),W) ) * ( MATMUL(transpose(U),(W*A)/(UV + E)) )
     ! Vaux = ( V/MATMUL(transpose(U),W) ) * ( MATMUL(transpose(U),(W*A)/(UV + E)) )

     ! Check NaN in V
     DO i = 1, rank
        DO j = 1, col
           IF (isnan( V(i,j) )) THEN
              write(*,'(A,I3,A,I3,A,I3)') 'i:',i,'  j:',j, '  step:', steps
              STOP "NaN in V"
           ENDIF
        END DO
     END DO

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
     IF ( (KL < precision) .and. (maxUV <= 255) ) EXIT

     ! euclid = Euclidea(A, UV, row, col)
  END DO
  
  WRITE(*,*) ''
  WRITE(*,*) ' Norma infinito U: ', norm_infty(U, row, rank)
  WRITE(*,*) ' Norma infinito V: ', norm_infty(V, rank, col)

  WRITE(*,*) ''
  WRITE(*,'(A,I2)', advance='no') ' Passi: ', steps
  WRITE(*,'(A,F10.2)', advance='no') ' Precisione: ', precision 
  WRITE(*,*) 'KL Divergence: ', KL

END SUBROUTINE factor

SUBROUTINE checkUV(UV, row, col, maxUV)
  LOGICAL :: ZERO = .FALSE. , TOOBIG = .FALSE. , NAN = .FALSE.
  REAL maxUV
  integer row, col, i ,j
  REAL(KIND(0.d0)), DIMENSION(row,col) :: UV

  maxUV = 0

  DO i = 1, row
     DO j = 1, col
        IF ( isnan(UV(i,j)) ) THEN
           NAN = .TRUE.
           STOP 'NaN in UV'
        ELSE 
           IF ( UV(i,j) == 0 ) THEN
              ZERO = .TRUE.
           Else
              IF ( maxUV < UV(i,j) ) then
                 maxUV = UV(i,j)
                 IF ( UV(i,j) > 255 ) THEN
                    TOOBIG = .TRUE.
                 ENDIF
              ENDIF
           ENDIF
        ENDIF
     END DO
  END DO
  IF (ZERO) THEN
      WRITE(*,*) "Zero found"
  ENDIF
  IF (TOOBIG) THEN
     WRITE(*,'(A,F6.2)', advance='no') " max in UV: ", maxUV
  ELSE
     WRITE(*,'(A,F6.2)', advance='no') " UV nella media, max:", maxUV
  ENDIF

ENDSUBROUTINE checkUV

function KLdivergence (A, UV, W, row, col, eps) result(KLDiv)
  ! Calcola la divergenza di Kullback-Leibler pesata
  ! IMPLICIT NONE
  INTEGER row, col
  INTEGER, DIMENSION (row,col) :: A
  REAL(KIND(0.d0)), DIMENSION (row,col) :: UV, W, DIV
  INTEGER i, j
  REAL(KIND(0.d0)) KLDiv, eps, normU, normV, norm

  ! norm = normU*normV

  KLDiv = 0
  DO i = 1, row                 
     DO j = 1, col
        IF ( A(i,j) > 0 ) THEN ! SALTIAMO ELEMENTI 0 ?
           DIV(i,j) = log( A(i,j)/ ( UV(i,j) + eps ) ) 
        ENDIF
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
  REAL(KIND(0.d0)), DIMENSION (row,col) :: B
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
