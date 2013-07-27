SUBROUTINE createA ()
  ! Crea la matrice A le cui colonne sono i vector face
  ! le immagini sono 92x112 = 10304
  INTEGER n, m                     ! indice per le immagini
  INTEGER ( kind = 4 ) :: file_unit
  CHARACTER ( len=1024 ) :: filename
  CHARACTER ( len=1024 ) :: format_string

  INTEGER ( kind = 4 ) i,j,k
  INTEGER ( kind = 4 ) ierror
  INTEGER ( kind = 4 ) ios
  INTEGER ( kind = 4 ) nrow, ncol, maxg

  INTEGER :: row = 10304, col = 10                    ! righe e colonne (numero di immagini) di A
  INTEGER ( kind = 4 ), ALLOCATABLE, DIMENSION ( :, : ) :: readPGM, A 

  ALLOCATE ( A(row,col) )
  WRITE(*,*) 'Allocata matrice A: ', SHAPE(A)

  DO n=1,col
     IF (n < 10) then
        format_string = "(A4,I1,A4)"
     else
        format_string = "(A4,I2,A4)"
     ENDIF
     
     WRITE (filename,format_string) "img/",n,".pgm"
     ! PRINT *, trim(filename)

     CALL get_unit ( file_unit )
     ! Apro il file PGM
     OPEN ( UNIT=file_unit, FILE=filename, STATUS='old', IOSTAT=ios )
     IF ( ios /= 0 ) THEN
        WRITE ( *, '(a)' ) ' '
        WRITE ( *, '(a)' ) 'TEST - Fatal error!'
        WRITE ( *, '(a)' ) '  Could not open the file.'
        RETURN
     ENDIF

     CALL pgma_read_header ( file_unit, nrow, ncol, maxg )
     ! WRITE ( *, '(a)' ) ' '
     ! WRITE ( *, '(a,a12)' ) '  PGMA_READ_HEADER read the header. ', filename
     ! WRITE ( *, '(a)' ) ' '
     ! WRITE ( *, '(a,i8)' ) '  Number of rows of data =    ', nrow
     ! WRITE ( *, '(a,i8)' ) '  Number of columns of data = ', ncol
     ! WRITE ( *, '(a,i8)' ) '  Maximum G value =           ', maxg

     ! Lettura dati
     ALLOCATE ( readPGM(nrow,ncol) )
     ! WRITE(*,*) 'Alloco matrice readPGM per lettura dati: ', SHAPE(readPGM)
     CALL pgma_read_data ( file_unit, nrow, ncol, readPGM )

     ! Salvo g come vettore in A
     m = 1
     DO i = 1, nrow
        DO j = 1, ncol
           A(m,n) = readPGM(i,j)
           m = m+1
        END DO
     END DO
     
     DEALLOCATE (readPGM)

  END DO

  WRITE(*,*) ' Scritti vector face in A'! , SHAPE(A)

  DEALLOCATE (A)

  RETURN

END SUBROUTINE createA
