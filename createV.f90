SUBROUTINE createV(A,U,V, row, rank, col)
  ! Calcolo la matrice V
  INTEGER row, col, rank
  INTEGER (kind = 4), DIMENSION(row,col) :: A
  REAL, DIMENSION(row,rank) :: U
  REAL, DIMENSION(rank,col) :: V
  INTEGER i, j

  ! REAL, DIMENSION(rank,col) :: Va
  ! REAL, DIMENSION(rank,rank) :: Uk, invUk
  ! REAL, DIMENSION(rank) :: y

  CALL RANDOM_NUMBER(V)

  ! Salvo la sottomatrice U_k
  ! DO i = 1, rank
  !    DO j = 1, rank
  !       Uk(i,j) = U(i,j)
  !    END DO
  ! END DO

  ! CALL FINDinv(Uk,invUk, rank, error)
  ! WRITE(*,*) 'invertita Uk'

  ! DO j=1,col
  !    ! WRITE(*,*) 'Calcolo y'
  !    y = A(:,j)
  !    ! WRITE(*,*) 'MATMUL...'
  !    V(:,j)= MATMUL(invUk, y)
  !    ! WRITE(*,*) SHAPE(V(:,j)), SHAPE(y), SHAPE( A(:,j) )
  ! END DO

  ! Controllo NonNegatività di U
  DO i = 1, row
     DO j = 1, rank
        IF ( U(i,j) <= 0 ) THEN
           ! U(i,j) = 0
           STOP 'trovato elemento <= 0'
        ENDIF
     END DO
  END DO

END SUBROUTINE createV
