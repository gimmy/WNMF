SUBROUTINE createV(A,U,V, row, rank, col)
  ! Calcolo la matrice V
  INTEGER row, col, rank
  INTEGER (kind = 4), DIMENSION(row,col) :: A
  REAL, DIMENSION(row,rank) :: U
  REAL, DIMENSION(rank,col) :: V
  INTEGER i, j

  REAL, DIMENSION(rank,col) :: Va
  REAL, DIMENSION(rank,rank) :: Uk, invUk
  REAL, DIMENSION(rank) :: y
  REAL maxV

  ! Salvo la sottomatrice U_k
  DO i = 1, rank
     DO j = 1, rank
        Uk(i,j) = U(i,j)
     END DO
  END DO

  CALL FINDinv(Uk,invUk, rank, error)
  ! invUk = inv(Uk)
  ! CALL inv(Uk,invUk)
  WRITE(*,*) 'inversa di Uk calcolata'

  ! ! Controllo NonNegatività di invUk
  ! DO i = 1, rank
  !    DO j = 1, rank
  !       IF ( invUk(i,j) <= 0 ) THEN
  !          STOP 'trovato elemento <= 0 in invUk'
  !       ENDIF
  !    END DO
  ! END DO

  DO j=1,col
     ! Calcolo y
     DO i=1,rank
        y(i) = A(i,j)
     ENDDO
     ! WRITE(*,*) 'MATMUL...'
     ! WRITE(*,*) SHAPE(V(:,j)), SHAPE(invUk), SHAPE(y)
     V(:,j)= MATMUL(invUk, y)
  END DO

  ! Controllo NonNegatività di V
  maxV = 0
  DO i = 1, rank
     DO j = 1, col
        IF ( V(i,j) < 0 ) THEN
           ! STOP 'trovato elemento < 0 in V'
           ! write(*,*) 'trovato elemento < 0 in V, lo metto a 0'
           V(i,j) = 0
        ELSE
           ! STOP 'trovato elemento > 0 in V'
           IF ( maxV < V(i,j) ) then
              maxV = V(i,j)
           ENDIF
           k = k+1
        ENDIF
     END DO
  END DO

  write(*,'(A,I5,A,I5)') 'Non distruggerò V per ',k,' elementi giusti su ',rank*col
  write(*,*) 'V ha raggiunto quota ', maxV

END SUBROUTINE createV
