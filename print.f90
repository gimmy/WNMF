SUBROUTINE print(A,row,col)
  ! Scrivo su file A.pgm la matrice A
  ! (in maniera guardabile, cioè affiancando le immagini)
  INTEGER :: row, col
  INTEGER, dimension (row,col) :: A

  INTEGER n, m                     ! indici per le immagini
  INTEGER ( kind = 4 ) :: file_unit
  CHARACTER ( len=1024 ) :: filename
  CHARACTER ( len=1024 ) :: format_string

  INTEGER ( kind = 4 ) i,j,k
  INTEGER ( kind = 4 ) ierror
  INTEGER ( kind = 4 ) ios
  INTEGER ( kind = 4 ) nrow, ncol, maxg, totcol

  INTEGER ( kind = 4 ), ALLOCATABLE, DIMENSION (:,:) :: faces

  ! Dimensioni faccia singola
  nrow = 112
  ncol = 92

  totcol = ncol*col
  ALLOCATE ( faces(nrow,totcol) )

  write(*,*) 'faces', shape(faces)

  DO n=1,col 
     ! Scrivo volti in faces
     m = 1
     DO i = 1, nrow
        DO j = 1, ncol
           faces(i,j+(ncol*n)) = A(m,n)
           m = m+1
        END DO
     END DO

  ENDDO

  CALL pgma_write ( 'A.pgm', nrow, totcol, faces, ierror )
  WRITE(*,*) 'Scritta faces in A.pgm'

  DEALLOCATE ( faces )

END SUBROUTINE print
