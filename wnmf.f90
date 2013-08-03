PROGRAM wnmf

  IMPLICIT NONE

  ! righe e colonne (numero di immagini) di A
  INTEGER :: row = 10304, col = 10, rank = 49
  REAL (kind = 4), ALLOCATABLE, DIMENSION (:,:) :: W
  INTEGER (kind = 4), ALLOCATABLE, DIMENSION (:,:) :: A
  REAL, ALLOCATABLE, DIMENSION(:,:) :: U
  REAL, ALLOCATABLE, DIMENSION(:,:) :: V

  REAL, ALLOCATABLE, DIMENSION(:,:) :: UV

  ! INTEGER, parameter :: sigma = 30

  ALLOCATE ( A(row,col) )
  ALLOCATE ( W(row,col) )
  WRITE(*,*) 'Allocata matrice A e W: ', SHAPE(A)

  ! CALL read ( 'img_ascii.pgm' )

  CALL createAW (A, W, row, col)

  ALLOCATE ( U(row,rank) )
  ALLOCATE ( V(rank,col) )

  ! ALLOCATE ( UV(row,col) )

  CALL factor (A, W, row, col, rank, U, V) !, UV)
  WRITE(*,*) 'Fattorizzazione conclusa.'
  DEALLOCATE (U)
  DEALLOCATE (V)

  ! DEALLOCATE (UV)

  DEALLOCATE (A)
  DEALLOCATE (W)

  ! WRITE ( *, '(a)' ) ' '
  ! CALL timestamp ( )

  ! stop

END PROGRAM wnmf
