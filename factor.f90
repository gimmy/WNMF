SUBROUTINE factor (A, row, col, rank)
  ! Fattorizzazione NonNegativa di A
  INTEGER row, col
  INTEGER ( kind = 4 ) A(row,col)
  INTEGER rank
  INTEGER i, j, steps
  INTEGER, parameter :: sigma = 30
  REAL (kind = 4) d
  REAL, parameter :: eps = 1e-10, precision = 1e3 !1e-2
  REAL KLDiv, euclid

  ! Creo matrici random U e V
  REAL, DIMENSION(row,rank) :: U
  REAL, DIMENSION(rank,col) :: V, Va
  ! Creo la matrice dei pesi
  REAL, DIMENSION(row,col) :: W, E, UV

  ! CALL RANDOM_SEED
  CALL RANDOM_NUMBER(U) ! put random numbers
  CALL RANDOM_NUMBER(V) 

  ! Creo la matrice dei pesi e la matrice 
  ! Epsilon per evitare la divisione per zero
  DO i = 1, row
     DO j = 1, col

        d = (i-56.5)**2  + (j-46.5)**2 ! distanza^2 dal centro
        W(i,j) = exp(-d/(sigma**2))
        E(i,j) = eps

     END DO
  END DO

  ! A*B is the Hadamard product
  ! A/B is the Hadamard division

  ! AUX = (W*A)/(UV + E)
  ! WRITE ( *,* ) '  AUX ha dimensione: ', SHAPE(AUX)
  ! WRITE ( *,* ) '   tU ha dimensione: ', SHAPE(transpose(U))

  steps = 0
  UV = MATMUL(U,V)
  DO
     steps = steps+1

     ! V1 = MATMUL( transpose(U),(W*A)/(UV + E) )
     ! V1 = ( V/MATMUL(transpose(U),W) ) * V1

     ! U = ( U/MATMUL(W,transpose(V)) ) * ( MATMUL((W*A)/(UV + E),transpose(V)) )
     ! Va = ( V/MATMUL(transpose(U),W) ) * ( MATMUL(transpose(U),(W*A)/(UV + E)) )
     ! V = Va

     UV = MATMUL(U,V)

     V = ( V/MATMUL(transpose(U),W) ) * ( MATMUL(transpose(U),(W*A)/(UV + E)) )
     CALL KLdivergence(A, UV, W, row, col, KLDiv)
     IF ( KLDiv < precision ) exit
     ! WRITE (*,*) 'KL Div [V update]: ', KLDiv

     ! U = ( U/MATMUL(W,transpose(V)) ) * ( MATMUL((W*A)/(UV + E),transpose(V)) )
     ! CALL KLdivergence(A, UV, W, row, col, KLDiv)
     ! IF ( KLDiv < precision ) exit
     ! WRITE (*,*) 'KL Div [U update]: ', KLDiv

     ! CALL Euclidiv(A, UV, row, col, euclid)
     ! IF ( euclid < precision ) exit
     
  END DO

  WRITE (*,*) 'Precision: ', precision 
  WRITE (*,*) 'KL Divergence: ', KLDiv
  WRITE(*,*) 'Passi: ', steps

END SUBROUTINE factor

SUBROUTINE KLdivergence (A, UV, W, row, col, KLDiv)
  ! Calcola la divergenza di Kullback-Leibler pesata
  IMPLICIT NONE
  INTEGER row, col
  INTEGER, DIMENSION (row,col) :: A
  REAL, DIMENSION (row,col) :: UV, W, AUX
  INTEGER i, j
  REAL KLDiv

  KLDiv = 0
  DO i = 1, row                 
     DO j = 1, col
        AUX(i,j) = log( A(i,j)/UV(i,j) )
     END DO
  END DO

  AUX = W * ( A * AUX - A + UV )

  DO i = 1, row
     DO j = 1, col
        KLDiv = KLDiv + AUX(i,j)
        IF ( KLDiv < 0 ) THEN
           WRITE (*,*) 'KL sotto zero: ', KLDiv
        END IF
     END DO
  END DO

  ! WRITE (*,*) 'KL Divergence =', KLDiv
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
