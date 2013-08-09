PROGRAM wnmf

  IMPLICIT NONE

  ! righe e colonne (numero di immagini) di A
  INTEGER :: row = 10304, col = 100, rank = 49
  REAL (kind = 4), ALLOCATABLE, DIMENSION (:,:) :: W
  INTEGER (kind = 4), ALLOCATABLE, DIMENSION (:,:) :: A

  REAL, ALLOCATABLE, DIMENSION(:,:) :: UV

  INTEGER, parameter :: maxiter = 55
  INTEGER, parameter :: sigma = 30
  CHARACTER (len = 10) filename
  INTEGER ( kind = 4 ) ierror
  REAL maxUV

  WRITE(*,'(A,I3,A,I2)') 'Immagini: ',col,', rango fattorizzazione: ',rank

  ALLOCATE ( A(row,col) )
  ALLOCATE ( W(row,col) )
  ! WRITE(*,*) 'Allocata matrice A e W'!, SHAPE(A)

  ! CALL read ( 'img_ascii.pgm' )

  CALL createAW (A, W, row, col)

  ! filename = 'A.pgm'
  ! CALL print(A,row,col,filename)

  ALLOCATE ( UV(row,col) )

  WRITE(*,*) 'Allocata matrice UV.'

  CALL factor (A, W, row, col, rank, UV, maxUV, maxiter)
  WRITE(*,*) 'Fattorizzazione conclusa.'

  IF ( maxUV < 255 ) THEN
     UV = int( UV )                ! cast a int
     filename = 'UV.pgm'
     CALL print(UV,row,col,filename)
  ENDIF

  DEALLOCATE (UV)

  DEALLOCATE (A)
  DEALLOCATE (W)

  ! CALL timestamp ( )

END PROGRAM wnmf
