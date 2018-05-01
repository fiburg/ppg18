PROGRAM ppg_2018_task_x
    IMPLICIT NONE

    INTEGER, PARAMETER :: RESOLUTION = 8
    INTEGER, PARAMETER :: seed = 528491
    INTEGER, PARAMETER :: MAX_SIZE = 10000
    INTEGER, PARAMETER :: MAX_RANGE = 255

    INTEGER :: i, j
    INTEGER(KIND=RESOLUTION) :: avg_red, avg_green, avg_blue

    TYPE :: rgb
        INTEGER(KIND=RESOLUTION),DIMENSION(MAX_SIZE,MAX_SIZE) :: red
        INTEGER(KIND=RESOLUTION),DIMENSION(MAX_SIZE,MAX_SIZE) :: green
        INTEGER(KIND=RESOLUTION),DIMENSION(MAX_SIZE,MAX_SIZE) :: blue
    END TYPE rgb

    TYPE(rgb) :: pixels

    ! Initialisation
    call srand(seed)
	
	! switch inner and outer loop
    DO j=1,MAX_SIZE
       DO i=1,MAX_SIZE
           pixels%red(i,j)     = int(rand()*MAX_RANGE, KIND=RESOLUTION)
           pixels%green(i,j)   = int(rand()*MAX_RANGE, KIND=RESOLUTION)
           pixels%blue(i,j)    = int(rand()*MAX_RANGE, KIND=RESOLUTION)
       END DO
    END DO
    avg_red     = 0
    avg_green   = 0
    avg_blue    = 0
    
    ! 0.3 s faster than the do loop
    ! avg_red = sum(pixels%red)
    ! avg_green = sum(pixels%green)
    ! avg_blue = sum(pixels%blue)
    
    ! Calculate average values
    ! switch inner and outer loop
    DO j=1,MAX_SIZE
    	DO i=1,MAX_SIZE
	    	avg_red = avg_red + pixels%temp_red(i,j)
        	avg_green = avg_green + pixels%green(i,j)
            avg_blue = avg_blue + pixels%blue(i,j)
        End DO
    END DO

    WRITE(*,*) "Calculated average values: "
    WRITE(*,*) "Red: ", avg_red/(MAX_SIZE*MAX_SIZE)
    WRITE(*,*) "Green: ", avg_green/(MAX_SIZE*MAX_SIZE)
    WRITE(*,*) "Blue: ", avg_blue/(MAX_SIZE*MAX_SIZE)

END PROGRAM ppg_2018_task_x