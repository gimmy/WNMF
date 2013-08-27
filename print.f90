SUBROUTINE printA(A,row,col,filename)
  ! Scrivo su file pgm la matrice B
  ! (in maniera guardabile, cioè affiancando le immagini)
  INTEGER :: row, col
  INTEGER, dimension (row,col) :: A

  INTEGER n, m                     ! indici per le immagini
  CHARACTER (len = 10) filename

  INTEGER i,j
  INTEGER ( kind = 4 ) ierror

  INTEGER, parameter :: nrow = 112, ncol= 92
  INTEGER, save :: maxg = 0, totmax, totcol

  INTEGER, dimension(nrow, ncol) :: sface
  INTEGER, dimension (nrow,ncol*col) :: faces

  ! Dimensioni faccia singola
  ! nrow = 112
  ! ncol = 92
  ! maxg = 0

  totcol = ncol*col

  write(*,*) 'faces', shape(faces)

  totmax = maxg
  DO n=1,col 
     ! Scrivo volti in faces
     m = 1
     DO i = 1, nrow
        DO j = 1, ncol
           faces( i,j+(ncol*(n-1)) ) = A(m,n)
           sface( i,j ) = A(m,n)
           IF ( sface(i,j) > maxg ) then
              maxg = sface(i,j)
           ENDIF
           m = m+1
        END DO
     END DO

     CALL pgma_check_data ( nrow, ncol, maxg, sface, ierror )
     if ( maxg > totmax ) then
        totmax = maxg
     endif
  ENDDO

  CALL pgma_check_data ( nrow, ncol, totmax, faces, ierror )

  CALL pgma_write ( filename, nrow, totcol, faces, ierror )
  WRITE(*,'(A,A)') 'Scritta faces in ', filename

END SUBROUTINE printA

SUBROUTINE print(B,row,col,filename)
  ! Scrivo su file pgm la matrice B
  ! (in maniera guardabile, cioè affiancando le immagini)
  INTEGER :: row, col
  REAL(KIND(0.d0)), dimension (row,col) :: B
  INTEGER, dimension (row,col) :: G

  INTEGER n, m                     ! indici per le immagini
  CHARACTER (len = 10) filename

  INTEGER i,j
  INTEGER ( kind = 4 ) ierror

  INTEGER, parameter :: nrow = 112, ncol= 92
  INTEGER, save :: maxg = 0, totmax, totcol

  INTEGER, dimension(nrow, ncol) :: sface
  INTEGER, dimension (nrow,ncol*col) :: faces

  ! Dimensioni faccia singola
  ! nrow = 112
  ! ncol = 92
  ! maxg = 0

  totcol = ncol*col

  write(*,*) 'faces', shape(faces)

  totmax = maxg
  DO n=1,col 
     ! Scrivo volti in faces
     m = 1
     DO i = 1, nrow
        DO j = 1, ncol
           G(m,n) = int ( B(m,n) )
           faces( i,j+(ncol*(n-1)) ) = G(m,n)
           sface( i,j ) = G(m,n)
           IF ( sface(i,j) > maxg ) then
              maxg = sface(i,j)
           ENDIF
           m = m+1
        END DO
     END DO

     CALL pgma_check_data ( nrow, ncol, maxg, sface, ierror )
     if ( maxg > totmax ) then
        totmax = maxg
     endif
  ENDDO

  CALL pgma_check_data ( nrow, ncol, totmax, faces, ierror )

  CALL pgma_write ( filename, nrow, totcol, faces, ierror )
  WRITE(*,'(A,A)') 'Scritta faces in ', filename

END SUBROUTINE print


SUBROUTINE simpleprint(B,row,col,filename)
  ! Scrivo su file pgm la matrice int(B) così com'è

  INTEGER :: row, col
  REAL(KIND(0.d0)), dimension (row,col) :: B
  INTEGER, dimension (row,col) :: G

  INTEGER n, m                     ! indici per le immagini
  CHARACTER ( len = 80 ) filename
  INTEGER i,j, max
  INTEGER ( kind = 4 ) ierror

  ! Stampo a video
  ! write (*,*) ''
  ! Write(*,'(A)') ' B: '
  DO i = 1, row
     DO j = 1, col        
        G(i,j) = int ( B(i,j) )
        ! write (*,'(A,I4)',advance='no') ' ', G(i,j)
        IF ( max < G(i,j) ) THEN
            max = G(i,j)
         ENDIF
     END DO
     ! write(*,*) ' '
  END DO

  CALL pgma_check_data ( row, col, max, G, ierror )

  CALL pgma_write ( filename, row, col, G, ierror )
  WRITE(*,'(A,A)') ' Matrice scritta in ', filename

END SUBROUTINE simpleprint
