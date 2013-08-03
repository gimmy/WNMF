SUBROUTINE factor (A, W, row, col, rank, U, V, iter)
  ! Fattorizzazione NonNegativa pesata di A
  ! Fattorizza A in UV con matrice dei pesi W

  ! A*B is the Hadamard product
  ! A/B is the Hadamard division

  INTEGER row, col, rank
  INTEGER (kind = 4), DIMENSION(row,col) :: A
  REAL, DIMENSION(row,col) :: W, UV
  REAL, DIMENSION(row,rank) :: U
  REAL, DIMENSION(rank,col) :: V, Vaux

  INTEGER i, j, steps, iter
  REAL, parameter :: eps = 1e-5, precision = 1e4
  REAL (kind = 4) KLdivergence, KL, Euclidea, euclid

  REAL norm_infty, normU, normV

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
  ! CALL RANDOM_NUMBER(V)
  CALL createV(A,U,V, row, rank, col)

  UV = MATMUL(U,V)
  WRITE(*,*) 'Calcolata UV'

  WRITE (*,*) 'Inizio fattorizzazione...'
  DO steps = 1, iter
     ! Aggiorno U
     U = ( U/MATMUL(W,transpose(V)) ) * ( MATMUL((W*A)/(UV + E),transpose(V)) )

     IF ( steps == iter ) THEN
        UV = MATMUL(U,V)
        KL = KLdivergence(A, UV, W, row, col, eps)
        WRITE (*,*) 'KL Div [U update]: ', KL
     ENDIF

     ! Aggiorno matrice V
     V = ( V/MATMUL(transpose(U),W) ) * ( MATMUL(transpose(U),(W*A)/(UV + E)) )
     ! Vaux = ( V/MATMUL(transpose(U),W) ) * ( MATMUL(transpose(U),(W*A)/(UV + E)) )

     ! IF ( steps == iter ) THEN
     !    UV = MATMUL(U,V)
     !    KL = KLdivergence(A, UV, W, row, col, eps)
     !    WRITE (*,*) 'KL Div [V update]: ', KL
     ! ENDIF

     ! ! Check Non NaN in U
     ! DO i = 1, row
     !    DO j = 1, rank
     !       IF (isnan( U(i,j) )) THEN
     !          write(*,'(A,I3,A,I3)') 'i:',i,' j:',j
     !          STOP "NaN in U"
     !       ENDIF
     !    END DO
     ! END DO

     ! V = Vaux
     UV = MATMUL(U,V)
     KL = KLdivergence(A, UV, W, row, col, eps)
     IF ( KL < precision ) exit
     IF ( steps == iter ) THEN
        WRITE (*,*) 'KL Div [UV update]: ', KL
     ENDIF

     ! ! WRITE(*,*) 'Ricalcolata UV'
     ! HELP = .FALSE.
     ! ! Check Zero in UV
     ! DO i = 1, row
     !    DO j = 1, col
     !       IF ( UV(i,j) == 0 ) THEN
     !          HELP = .TRUE.
     !       ENDIF
     !    END DO
     ! END DO
     ! IF (HELP) THEN
     !    STOP "Zero in UV!"
     ! ENDIF

     ! euclid = Euclidea(A, UV, row, col)


     normU = norm_infty(U, row, rank)
     normV = norm_infty(V, rank, col)
     WRITE(*,*) 'Norma infinito U: ', normU
     WRITE(*,*) 'Norma infinito V: ', normV

  END DO

  WRITE(*,*) ''
  WRITE(*,*) 'Precision: ', precision 
  WRITE(*,*) 'KL Divergence: ', KL
  WRITE(*,*) 'Passi: ', steps

END SUBROUTINE factor


function KLdivergence (A, UV, W, row, col, eps) result(KLDiv)
  ! Calcola la divergenza di Kullback-Leibler pesata
  ! IMPLICIT NONE
  INTEGER row, col
  INTEGER, DIMENSION (row,col) :: A
  REAL, DIMENSION (row,col) :: UV, W, DIV
  INTEGER i, j
  REAL KLDiv, eps, normU, normV, norm

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
