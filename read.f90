subroutine read ()
  ! Legge un file pgm (ascii) in input

  IMPLICIT NONE

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

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  PGMA_READ_HEADER read the header.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  Number of rows of data =    ', nrow
  write ( *, '(a,i8)' ) '  Number of columns of data = ', ncol
  write ( *, '(a,i8)' ) '  Maximum G value =           ', maxg

  allocate ( g(nrow,ncol) )

  CALL pgma_read_data ( file_unit, nrow, ncol, g )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  PGMA_READ_DATA read the data.'

  ! write ( *, '(a)' ) ' '
  ! write ( *, '(a)' ) '  Sample data:'
  ! write ( *, '(a)' ) ' '

  ! do k = 1, 10
  !   i = ( ( 10 - k ) * 1 + ( k - 1 ) * nrow ) / ( 10 - 1 )
  !   j = ( ( 10 - k ) * 1 + ( k - 1 ) * ncol ) / ( 10 - 1 )
  !   write ( *, '(i4,2x,i4,2x,i6)' ) i, j, g(i,j)
  ! end do

  ! CALL pgma_check_data ( nrow, ncol, maxg, g, ierror )

  ! if ( ierror /= 0 ) then
  !   write ( *, '(a)' ) ' '
  !   write ( *, '(a)' ) 'TEST02'
  !   write ( *, '(a,i8)' ) '  The data was not accepted by PGMA_CHECK_DATA.'
  !   return
  ! end if

  ! write ( *, '(a)' ) ' '
  ! write ( *, '(a)' ) '  The data was accepted by PGMA_CHECK_DATA.'

  deallocate ( g )

  return

END SUBROUTINE read
