SUBROUTINE factor (A, row, col, rank)
  ! Fattorizzazioni NonNegativa di A
  INTEGER row, col
  INTEGER ( kind = 4 ) A(row,col)
  INTEGER rank
  INTEGER i, j
  INTEGER, parameter :: sigma = 30
  REAL (kind = 4) d
  REAL, parameter :: eps = 1e-7, precision = 1e-8
  REAL KLDiv

  ! Creo matrici random U e V
  REAL, DIMENSION(row,rank) :: U, tU, U1
  REAL, DIMENSION(rank,col) :: V, tV, AUX_V, V1
  ! Creo la matrice dei pesi
  REAL, DIMENSION(row,col) :: W, E, AUX, UV

  ! CALL RANDOM_SEED
  CALL RANDOM_NUMBER(U) ! put a random numbers
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
  WRITE ( *, '(a)' ) '  Create matrici W e E.'
  ! Aggiornamento di V
  tU = transpose(U)
  tV = transpose(V)
  UV = MATMUL(U,V)
  WRITE ( *, '(a)' ) '  Creata UV.'
  ! AUX_V = MATMUL(tU,W)

  ! DO i = 1, rank                
  !    DO j = 1, col

  !       V(i,j) = V(i,j)/AUX_V(i,j)

  !    END DO
  ! END DO

  ! A*B is the Hadamard product
  ! A/B is the Hadamard division ?

  ! KLDiv = 10
  DO
     V1 = MATMUL(tU,(W*A)/(UV + E))
     WRITE ( *, '(a)' ) '  Sono vivo e ho fatto un passo verso V1.'
     V1 = ( V/MATMUL(tU,W) ) * V1
     WRITE ( *, '(a)' ) '  Sono vivo e ho fatto un altro passo verso V1.'
     U1 = U
     ! V1 = ( V/MATMUL(tU,W) ) * ( MATMUL(tU,(W*A)/(UV + E)) )
     ! U1 = ( U/MATMUL(W,tV) ) * ( MATMUL((W*A)/(UV + E),tV) )
     WRITE ( *, '(a)' ) '  Sono vivo.'
     CALL KLdivergence(A,U1,V1,W,row,col,rank,KLDiv)
     IF ( KLDiv > precision ) exit
  END DO

  WRITE (*,*) 'Precision: ', precision 
  WRITE (*,*) 'KL Divergence: ', KLDiv
  WRITE (*,*) 'Uscito con errore: ', precision - KLDiv

END SUBROUTINE factor

SUBROUTINE KLdivergence (A, U, V, W, row, col, rank, KLDiv)
  ! Calcola la divergenza di Kullback-Leibler pesata
  IMPLICIT NONE
  INTEGER row, col, rank
  INTEGER, DIMENSION (row,col) :: A
  REAL, DIMENSION (row,rank) :: U
  REAL, DIMENSION (rank,col) :: V
  REAL, DIMENSION (row,col) :: W, UV, A1
  INTEGER i, j
  REAL KLDiv

  UV = MATMUL(U,V)
  DO i = 1, row                 ! TODO: evitare questo ciclo
     DO j = 1, col
        A1(i,j) = log( A(i,j)/UV(i,j) )
     END DO
  END DO

  A1 = A*A1 - A + MATMUL(U,V)
  A1 = W*A1

  DO i = 1, row
     DO j = 1, col
        KLDiv = abs(KLDiv) + A1(i,j)
        WRITE (*,*) 'Sono vivo e con KL: ', KLDiv
     END DO
  END DO

  WRITE (*,*) 'KL Divergence = ', KLDiv
  RETURN

END SUBROUTINE KLdivergence
