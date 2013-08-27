PROGRAM wnmf

  IMPLICIT NONE

  ! righe e colonne (numero di immagini) di A
  INTEGER :: row = 10304, col = 10, rank = 9
  REAL (KIND(0.d0)), ALLOCATABLE, DIMENSION (:,:) :: W, UV
  INTEGER (kind = 4), ALLOCATABLE, DIMENSION (:,:) :: A

  INTEGER, parameter :: maxiter = 60, sigma = 30
  CHARACTER (len = 10) filename
  INTEGER (kind = 4) ierror
  REAL maxUV
  INTEGER i,j

  WRITE(*,'(A,I3,A,I2)') 'Immagini: ',col,', rango fattorizzazione: ',rank

  ALLOCATE ( A(row,col) )
  ALLOCATE ( W(row,col) )
  ! WRITE(*,*) 'Allocata matrice A e W'!, SHAPE(A)

  ! CALL read ( 'img_ascii.pgm' )

  CALL createAW (A, W, row, col)

  filename = 'A.pgm'
  CALL printA(A,row,col,filename)

  ALLOCATE ( UV(row,col) )

  WRITE(*,*) 'Allocata matrice UV.'

  CALL factor (A, W, row, col, rank, UV, maxUV, maxiter)
  WRITE(*,*) 'Fattorizzazione conclusa.'

  IF ( maxUV <= 255 ) THEN
     UV = int( UV )                ! cast a int
     filename = 'UV.pgm'
     CALL print(UV,row,col,filename)
  ENDIF

  ! Fattorizzazione senza pesi
  WRITE(*,*) 'Fattorizzazione non pesata'
  ! Creo la matrice dei pesi
  WRITE ( *, '(a)', advance='no' ) '  Creo matrice dei pesi..'
  DO i = 1, row
     DO j = 1, col
        W(i,j) = 1
     END DO
  END DO

  WRITE ( *, '(a)', advance='no' ) '  Factor... '
  CALL factor (A, W, row,col,rank,UV,maxUV,maxiter)
  WRITE ( *, '(a)' ) ' '
  WRITE ( *, '(a)' ) '  Fattorizzazione fatta.'

  filename = 'UV_noW.pgm'
  CALL print (UV,row,col,filename)

  DEALLOCATE (UV)

  DEALLOCATE (A)
  DEALLOCATE (W)

  ! CALL timestamp ( )

END PROGRAM wnmf
