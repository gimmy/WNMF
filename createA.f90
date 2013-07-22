SUBROUTINE test ()
  INTEGER n                     ! indice per le immagini
  CHARACTER ( len=1024 ) :: filename
  CHARACTER ( len=1024 ) :: format_string
  INTEGER ( kind = 4 ) file_unit
  ! INTEGER ( kind = 4 ), ALLOCATABLE, DIMENSION ( :, : ) :: g

  INTEGER lvec                             ! lunghezza del vettore v
  INTEGER, DIMENSION (:), ALLOCATABLE :: vec   ! Dichiaro il vettore v

  DO n=1,10
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

     WRITE ( *, '(a)' ) ' '
     WRITE ( *, '(a,a8)' ) '  PGMA_READ_HEADER read the header.', filename
     WRITE ( *, '(a)' ) ' '
     WRITE ( *, '(a,i8)' ) '  Number of rows of data =    ', nrow
     WRITE ( *, '(a,i8)' ) '  Number of columns of data = ', ncol
     WRITE ( *, '(a,i8)' ) '  Maximum G value =           ', maxg

  END DO

  lvec = row_num * col_num
  ALLOCATE ( vec(lvec) )

END SUBROUTINE test

SUBROUTINE read_data_in_vector ( file_in_unit, row_num, col_num, v )

!********************************************************************
!
!! PGMA_READ_DATA reads the data in an ASCII PGM file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 

  IMPLICIT NONE

  integer  col_num
  integer  row_num

  logical done
  integer ( kind = 4 ) file_in_unit
  ! integer ( kind = 4 ) g(row_num,col_num)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  ! integer ( kind = 4 ) j
  character ( len = 80 ) string


  INTEGER leng_vector
  INTEGER, DIMENSION (:), ALLOCATABLE :: v   ! Dichiaro il vettore v

  ! leng_vector = row_num * col_num
  ! ALLOCATE ( v(leng_vector) )

  ierror = 0
  done = .true.
  string = ' '

  DO i = 1, leng_vector
     call getint ( done, ierror, file_in_unit, v(i), string )

     IF ( ierror /= 0 ) then
        close ( unit = file_in_unit )
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PGMA_READ_DATA - Fatal error!'
        write ( *, '(a)' ) '  Problem reading G data.'
        stop
     END IF

  END DO

  ! do i = 1, row_num
  !   do j = 1, col_num

  !     call getint ( done, ierror, file_in_unit, g(i,j), string )

  !     if ( ierror /= 0 ) then
  !       close ( unit = file_in_unit )
  !       write ( *, '(a)' ) ' '
  !       write ( *, '(a)' ) 'PGMA_READ_DATA - Fatal error!'
  !       write ( *, '(a)' ) '  Problem reading G data.'
  !       stop
  !     end if

  !   end do
  ! end do

  RETURN
END SUBROUTINE read_data_in_vector


SUBROUTINE createA ()
  ! Crea la matrice A
  IMPLICIT NONE

  INTEGER n                     ! indice per le immagini

  CHARACTER ( len = 80 ) :: file_name = 'img_ascii.pgm'
  INTEGER ( kind = 4 ) file_unit
  INTEGER ( kind = 4 ), allocatable, dimension ( :, : ) :: g
  INTEGER ( kind = 4 ) i
  INTEGER ( kind = 4 ) ierror
  INTEGER ( kind = 4 ) ios
  INTEGER ( kind = 4 ) j
  INTEGER ( kind = 4 ) k
  INTEGER ( kind = 4 ) maxg
  INTEGER ( kind = 4 ) ncol
  INTEGER ( kind = 4 ) nrow

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) '  Apro i file...'



END SUBROUTINE createA
