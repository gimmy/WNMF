SUBROUTINE createA ()
  ! Crea la matrice A le cui colonne sono i vector face
  INTEGER n                     ! indice per le immagini
  INTEGER ( kind = 4 ) :: file_unit
  CHARACTER ( len=1024 ) :: filename
  CHARACTER ( len=1024 ) :: format_string

  INTEGER :: row, col = 10                    ! righe e colonne di A (numero di immagini)
  INTEGER ( kind = 4 ), ALLOCATABLE, DIMENSION ( :, : ) :: g ! matrice A

  DO n=1,col
     IF (n < 10) then
        format_string = "(A4,I1,A4)"
     else
        format_string = "(A4,I2,A4)"
     ENDIF
     
     WRITE (filename,format_string) "img/",n,".pgm"
     ! PRINT *, trim(filename)

     CALL get_unit ( file_unit )

     OPEN ( UNIT=file_unit, FILE=filename, STATUS='old', IOSTAT=ios )
     IF ( ios /= 0 ) THEN
        WRITE ( *, '(a)' ) ' '
        WRITE ( *, '(a)' ) 'TEST - Fatal error!'
        WRITE ( *, '(a)' ) '  Could not open the file.'
        RETURN
     ENDIF

     CALL pgma_read_header ( file_unit, nrow, ncol, maxg )

     ! WRITE ( *, '(a)' ) ' '
     ! WRITE ( *, '(a,a12)' ) '  PGMA_READ_HEADER read the header.', filename
     ! WRITE ( *, '(a)' ) ' '
     ! WRITE ( *, '(a,i8)' ) '  Number of rows of data =    ', nrow
     ! WRITE ( *, '(a,i8)' ) '  Number of columns of data = ', ncol
     ! WRITE ( *, '(a,i8)' ) '  Maximum G value =           ', maxg

  END DO

  row = row_num * col_num
  ALLOCATE ( g(row,col) )

  CALL read_data ( file_unit, row, col, g )

  DEALLOCATE ( g )

  RETURN

END SUBROUTINE createA


SUBROUTINE read_data ( file_in_unit, row_num, col_num, A )

!********************************************************************
!
!! PGMA_READ_DATA reads the data in an ASCII PGM file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 

  IMPLICIT NONE

  INTEGER  col_num
  INTEGER  row_num

  LOGICAL done
  INTEGER ( kind = 4 ) file_in_unit
  INTEGER ( kind = 4 ) A(row_num,col_num)
  INTEGER ( kind = 4 ) i
  INTEGER ( kind = 4 ) ierror
  INTEGER ( kind = 4 ) j
  CHARACTER ( len = 80 ) string

  ierror = 0
  done = .TRUE.
  string = ' '

  DO i = 1, row_num
     DO j = 1, col_num

        CALL getint ( done, ierror, file_in_unit, A(i,j), string )

        IF ( ierror /= 0 ) THEN
           CLOSE ( unit = file_in_unit )
           WRITE ( *, '(a)' ) ' '
           WRITE ( *, '(a)' ) 'PGMA_READ_DATA - Fatal error!'
           WRITE ( *, '(a)' ) '  Problem reading G data.'
           ! STOP
        END IF

     END DO
  END DO

  RETURN

END SUBROUTINE read_data
