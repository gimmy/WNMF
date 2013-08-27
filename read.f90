MODULE Readmodule
IMPLICIT NONE
CONTAINS
  SUBROUTINE read ( file_name, g, nrow, ncol, maxg )
    ! Legge un file pgm (ascii) in input
    ! - per test su singolo file
    IMPLICIT NONE

    CHARACTER ( len = 80 ) :: file_name
    INTEGER ( kind = 4 ) file_unit
    INTEGER ( kind = 4 ), allocatable, dimension ( :, : ) :: g
    ! INTEGER ( kind = 4 ) i, j, k
    INTEGER ( kind = 4 ) ierror
    INTEGER ( kind = 4 ) ios
    INTEGER :: maxg !, maxiter = 68, rank = 15

    INTEGER nrow, ncol

    ! REAL (KIND(0.d0)), ALLOCATABLE, DIMENSION (:,:) :: localW, UV
    ! INTEGER, parameter :: sigma = 30
    ! REAL (KIND(0.d0)) d, maxUV

    WRITE ( *, '(a)' ) ' '
    WRITE ( *, '(a)' ) '  PGMA_READ reads an ASCII PGM file.'

    CALL get_unit ( file_unit )

    OPEN ( unit = file_unit, file = file_name, status = 'old', iostat = ios )

    IF ( ios /= 0 ) THEN
       WRITE ( *, '(a)' ) ' '
       WRITE ( *, '(a)' ) 'TEST02 - Fatal error!'
       WRITE ( *, '(a)' ) '  Could not open the file.'
       return
    END IF

    CALL pgma_read_header ( file_unit, nrow, ncol, maxg )

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  PGMA_READ_HEADER read the header.'
    write ( *, '(a)' ) ' '
    write ( *, '(a,i8)' ) '  Number of rows of data =    ', nrow
    write ( *, '(a,i8)' ) '  Number of columns of data = ', ncol
    WRITE ( *, '(a,i8)' ) '  Maximum G value =           ', maxg

    ALLOCATE ( g(nrow,ncol) )

    CALL pgma_read_data ( file_unit, nrow, ncol, g )


    RETURN

  END SUBROUTINE read
END MODULE Readmodule

PROGRAM test
  USE Readmodule

  IMPLICIT NONE

  INTEGER row, col, max
  INTEGER :: rank = 15
  INTEGER i, j
  INTEGER, ALLOCATABLE, DIMENSION (:,:) :: A

  REAL(KIND(0.d0)), ALLOCATABLE, DIMENSION(:,:) :: localW, UV
  INTEGER, parameter :: sigma = 30, maxiter = 80
  REAL (KIND(0.d0)) d, maxUV

  CHARACTER ( len = 80 ) filename
  ! INTEGER (kind = 4) ierror

  filename = 'img_ascii.pgm'
  ! filename = 'feep.pgm'

  CALL read ( filename, A, row, col, max )

  filename = 'A.pgm'
  CALL simpleprint(A,row,col,max, filename)

  ! Avanti tutta !
  
  WRITE ( *, '(a)' ) ' '

  ! Creo la matrice dei pesi
  WRITE ( *, '(a)', advance='no' ) '  Creo matrice dei pesi..'
  ALLOCATE ( localW(row,col) )
  DO i = 1, row
     DO j = 1, col
        d = (i-56.5)**2  + (j-46.5)**2    ! distanza^2 dal centro
        localW(i,j) = exp(-d/(sigma**2))
     END DO
  END DO

  ALLOCATE ( UV(row,col) )

  WRITE ( *, '(a)', advance='no' ) '  Factor... '
  CALL factor (A, localW, row,col,rank,UV,maxUV,maxiter)
  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) '  Fattorizzazione fatta.'

  filename = 'UV.pgm'
  CALL simpleprint (UV,row,col,max,filename)

  DEALLOCATE ( A )
  DEALLOCATE ( UV )

END PROGRAM test
