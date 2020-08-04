MODULE READ_DATA
    IMPLICIT NONE
    !include 'r1279block.h'
    !Global var for input files
    INTEGER :: n_nodes, n_edges,n_edges_c,node_offset
    INTEGER, DIMENSION(:), ALLOCATABLE :: P_ini, P_fin, D, V
    INTEGER, DIMENSION(:), ALLOCATABLE :: P_ini_c, P_fin_c, D_c, V_c
    INTEGER, DIMENSION(:), ALLOCATABLE :: D_dist, D_cumul
    REAL*8, DIMENSION(:), ALLOCATABLE :: k_nn,c

    ! Global vars for dynamics
    INTEGER :: n_susceptible,n_infected,n_recovered, n_initial,n_steps_conf,n_cycles,active_links
    REAL :: r_infect,r_infect_origin,r_rehab
    INTEGER, DIMENSION(:),ALLOCATABLE :: susceptible,infected,recovered
    LOGICAL :: is_confinement
    CONTAINS
    SUBROUTINE READ_FROM_FILES()
        IMPLICIT NONE
        INTEGER i
        
        !print*,'hola'
        OPEN (11,FILE='parameters.dat',STATUS='old')
        READ(11,*) node_offset
        READ(11,*) n_initial
        READ(11,*) n_steps_conf
        READ(11,*) n_cycles
        READ(11,*) r_infect_origin
        READ(11,*) r_rehab
        READ(11,*) is_confinement
        close(11)

    END SUBROUTINE READ_FROM_FILES
END MODULE READ_DATA

