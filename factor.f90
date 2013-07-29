SUBROUTINE factor (A, W, row, col, rank)
  ! Fattorizzazione NonNegativa pesata di A
  INTEGER row, col
  INTEGER ( kind = 4 ) A(row,col)
  INTEGER rank
  INTEGER i, j, steps
  ! INTEGER, parameter :: sigma = 30
  REAL (kind = 4) d
  REAL, parameter :: eps = 1e-10, precision = 1e5
  REAL KLDiv, euclid

  ! Creo matrici random U e V
  REAL, DIMENSION(row,rank) :: U
  REAL, DIMENSION(rank,col) :: V, Va
  ! Creo la matrice dei pesi
  REAL, DIMENSION(row,col) :: W, E, UV

  ! CALL RANDOM_SEED
  CALL RANDOM_NUMBER(U) ! put random numbers
  CALL RANDOM_NUMBER(V) 

  ! Creo la matrice E per evitare la divisione per zero
  DO i = 1, row
     DO j = 1, col
        E(i,j) = eps
     END DO
  END DO

  ! A*B is the Hadamard product
  ! A/B is the Hadamard division

  steps = 0
  WRITE (*,*) 'Calcolo UV, inizio fattorizzazione...'
  DO
     steps = steps+1

     ! V1 = MATMUL( transpose(U),(W*A)/(UV + E) )
     ! V1 = ( V/MATMUL(transpose(U),W) ) * V1

     ! U = ( U/MATMUL(W,transpose(V)) ) * ( MATMUL((W*A)/(UV + E),transpose(V)) )
     ! Va = ( V/MATMUL(transpose(U),W) ) * ( MATMUL(transpose(U),(W*A)/(UV + E)) )
     ! V = Va

     UV = MATMUL(U,V)

     ! Aggiorno matrice V
     ! V = ( V/MATMUL(transpose(U),W) ) * ( MATMUL(transpose(U),(W*A)/(UV + E)) )
     ! CALL KLdivergence(A, UV, W, row, col, KLDiv,eps)
     ! IF ( KLDiv < precision ) exit
     ! WRITE (*,*) 'KL Div [V update]: ', KLDiv

     ! Aggiorno matrice U
     U = ( U/MATMUL(W,transpose(V)) ) * ( MATMUL((W*A)/(UV + E),transpose(V)) )
     CALL KLdivergence(A, UV, W, row, col, KLDiv, eps)
     IF ( KLDiv < precision ) exit
     WRITE (*,*) 'KL Div [U update]: ', KLDiv

     ! CALL Euclidiv(A, UV, row, col, euclid)
     ! IF ( euclid < precision ) exit
     
  END DO

  WRITE (*,*) ''
  WRITE (*,*) 'Precision: ', precision 
  WRITE (*,*) 'KL Divergence: ', KLDiv
  WRITE(*,*) 'Passi: ', steps

END SUBROUTINE factor

SUBROUTINE KLdivergence (A, UV, W, row, col, KLDiv, eps)
  ! Calcola la divergenza di Kullback-Leibler pesata
  ! IMPLICIT NONE
  INTEGER row, col
  INTEGER, DIMENSION (row,col) :: A
  REAL, DIMENSION (row,col) :: UV, W, AUX
  INTEGER i, j
  REAL KLDiv, eps

  KLDiv = 0
  DO i = 1, row                 
     DO j = 1, col
        AUX(i,j) = log( A(i,j)/UV(i,j) + eps)
     END DO
  END DO

  ! write ( *, '(a)' ) '  Sample data of W:'
  ! write ( *, '(a)' ) ' '
  ! do k = 1, 10
  !   ! i = ( ( 10 - k ) * 1 + ( k - 1 ) * nrow ) / ( 10 - 1 )
  !   ! j = ( ( 10 - k ) * 1 + ( k - 1 ) * ncol ) / ( 10 - 1 )
  !   write ( *, * ) k, W(k+1000,1)
  ! end do

  AUX = W * ( A * AUX - A + UV )

  DO i = 1, row
     DO j = 1, col
        KLDiv = KLDiv + AUX(i,j)
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
  RETURN

END SUBROUTINE KLdivergence

SUBROUTINE Euclidiv(A, UV, row, col, euclid)
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

END SUBROUTINE Euclidiv
