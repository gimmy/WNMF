SUBROUTINE createAW (A, W, row, col)
  ! Crea la matrice A le cui colonne sono i vector face
  ! le immagini sono 92x112 = 10304
  INTEGER :: row, col
  INTEGER, dimension (row,col) :: A
  REAL(KIND(0.d0)), dimension (row,col) :: W

  INTEGER n, m                     ! indice per le immagini
  INTEGER ( kind = 4 ) :: file_unit
  CHARACTER ( len=1024 ) :: filename
  CHARACTER ( len=1024 ) :: format_string

  INTEGER i,j,k
  INTEGER nrow, ncol, maxg
  INTEGER ( kind = 4 ) ierror
  INTEGER ( kind = 4 ) ios

  INTEGER ( kind = 4 ), ALLOCATABLE, DIMENSION (:,:) :: readPGM
  REAL(KIND(0.d0)), ALLOCATABLE, DIMENSION (:,:) :: localW
  LOGICAL DO_localW
  INTEGER, parameter :: sigma = 30

  ! ALLOCATE ( A(row,col) )
  ! ALLOCATE ( W(row,col) )
  ! WRITE(*,*) 'Allocata matrice A e W: ', SHAPE(A)

  DO_localW = .TRUE.

  DO n=1,col
     IF (n < 10) then
        format_string = "(A6,I1,A4)"
     else
        IF (n < 100) then
           format_string = "(A6,I2,A4)"
        else
           format_string = "(A6,I3,A4)"
        ENDIF
     ENDIF
     
     WRITE (filename,format_string) "faces/",n,".pgm"
     ! PRINT *, trim(filename)

     CALL get_unit ( file_unit )
     ! Apro il file PGM
     OPEN ( UNIT=file_unit, FILE=filename, STATUS='old', IOSTAT=ios )
     IF ( ios /= 0 ) THEN
        WRITE ( *, '(a)' ) ' '
        WRITE ( *, '(a)' ) 'Fatal error!'
        WRITE ( *, * ) '  Could not open the file. ', filename
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
     ! WRITE(*,*) 'Alloco matrice readPGM per lettura dati:',SHAPE(readPGM)
     CALL pgma_read_data ( file_unit, nrow, ncol, readPGM )

     IF ( DO_localW ) THEN
        ! Creo la matrice dei pesi
        ALLOCATE ( localW(nrow,ncol) )
        ! WRITE(*,*) 'Alloco matrice locale dei pesi localW:', SHAPE(localW)
        DO i = 1, nrow
           DO j = 1, ncol
              d = (i-56.5)**2  + (j-46.5)**2    ! distanza^2 dal centro
              localW(i,j) = exp(-d/(sigma**2))
           END DO
        END DO
        DO_localW = .FALSE.
     ENDIF

     ! Salvo readPGM come vettore colonna in A
     ! e salvo localW come vettore colonna in W
     m = 1
     DO i = 1, nrow
        DO j = 1, ncol
           A(m,n) = 255 - readPGM(i,j) ! salvo come negativi
           W(m,n) = localW(i,j)
           m = m+1
        END DO
     END DO
     
     DEALLOCATE (readPGM)

  END DO

  DEALLOCATE (localW)
  DO_localW = .TRUE.

  WRITE(*,*) 'Scritti vector face in A e pesi in W'
  WRITE(*,*) 'A:', SHAPE(A)
  WRITE(*,*) 'W:', SHAPE(W)

END SUBROUTINE createAW
