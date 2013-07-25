subroutine read ( file_name )
  ! Legge un file pgm (ascii) in input

  IMPLICIT NONE

  CHARACTER ( len = 80 ) :: file_name  ! = 'img_ascii.pgm'
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
  WRITE ( *, '(a)' ) '  PGMA_READ reads an ASCII PGM file.'

  CALL get_unit ( file_unit )

  OPEN ( unit = file_unit, file = file_name, status = 'old', iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'TEST02 - Fatal error!'
    write ( *, '(a)' ) '  Could not open the file.'
    return
  end if

  CALL pgma_read_header ( file_unit, nrow, ncol, maxg )

  ! write ( *, '(a)' ) ' '
  ! write ( *, '(a)' ) '  PGMA_READ_HEADER read the header.'
  ! write ( *, '(a)' ) ' '
  ! write ( *, '(a,i8)' ) '  Number of rows of data =    ', nrow
  ! write ( *, '(a,i8)' ) '  Number of columns of data = ', ncol
  ! WRITE ( *, '(a,i8)' ) '  Maximum G value =           ', maxg

  ALLOCATE ( g(nrow,ncol) )

  CALL pgma_read_data ( file_unit, nrow, ncol, g )

  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) '  PGMA_READ_DATA read the data.'

  CALL factor (g,nrow,ncol,30)
  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) '  Fattorizzazione fatta.'

  DEALLOCATE ( g )

  return

END SUBROUTINE read
