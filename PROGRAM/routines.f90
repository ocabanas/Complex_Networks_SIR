module thermodynamic_routines
    use READ_DATA
    IMPLICIT NONE
    contains
  SUBROUTINE INITIAL_INFECTED()
    IMPLICIT NONE
    INTEGER :: i, propose_infected
    REAL :: rnd
    ALLOCATE(susceptible(n_nodes),infected(n_nodes),recovered(n_nodes))
    DO i=1,n_nodes
        susceptible(i)=i
    END DO
    infected=0
    recovered=0
    n_susceptible=n_nodes
    n_infected=0
    n_recovered=0

    ! Random infection of some nodes 

    DO i=1,n_initial
        CALL RANDOM_NUMBER(rnd)
        propose_infected=int(rnd*(n_susceptible-1))+1
        n_infected=n_infected+1
        infected(n_infected)=susceptible(propose_infected)
        susceptible(propose_infected)=susceptible(n_susceptible)
        susceptible(n_susceptible)=0
        n_susceptible=n_susceptible-1
    END DO
  END SUBROUTINE

  SUBROUTINE INFECT_NODE_N()
    INTEGER :: origin,i,j,k,candidate,accepted_candidate,index_accepted
    REAL :: rnd
    LOGICAL :: susceptible_to_infect
    susceptible_to_infect=.FALSE.
    !
    ! CHOOSING A RANDOM INFECTED NODE AS ORIGIN
    ! 
    CALL RANDOM_NUMBER(rnd)
    origin=int(rnd*(n_infected-1))+1
    !Looping for all neighbours of the origin till we find an susceptible node
    IF (D(infected(origin)).gt.0) THEN
        DO j=P_ini(infected(origin)),P_fin(infected(origin))
            candidate=V(j)
            DO k=1,n_susceptible
                IF (candidate.eq.susceptible(k)) THEN
                    accepted_candidate=candidate
                    index_accepted=k
                    susceptible_to_infect=.TRUE.
                    exit
                END IF
            END DO
            IF(susceptible_to_infect.eqv..TRUE.) exit
        END DO
    END IF
  !
  ! IF RANDOM DON'T WORK, CHECK ALL OTHER INFECTED NODES
  !
    origin=1
      DO WHILE ((susceptible_to_infect.eqv..FALSE.).and.(origin.lt.n_infected-1))
          IF(susceptible_to_infect.eqv..TRUE.) exit
        !Looping for all neighbours of the origin till we find an susceptible node
          IF (D(infected(origin)).gt.0) THEN
              DO j=P_ini(infected(origin)),P_fin(infected(origin))
                candidate=V(j)
                  DO k=1,n_susceptible
                      IF (candidate.eq.susceptible(k)) THEN
                          accepted_candidate=candidate
                          index_accepted=k
                          susceptible_to_infect=.TRUE.
                          exit
                      END IF
                  END DO
                  IF(susceptible_to_infect.eqv..TRUE.) exit
              END DO
          END IF
        origin=origin+1
      END DO

      IF(susceptible_to_infect.eqv..TRUE.) THEN
          n_infected=n_infected+1
        infected(n_infected)=susceptible(index_accepted)
        susceptible(index_accepted)=susceptible(n_susceptible)
        susceptible(n_susceptible)=0
        n_susceptible=n_susceptible-1
    END IF
  END SUBROUTINE
  SUBROUTINE INFECT_NODE_C(cyc)
      INTEGER :: origin,i,j,k,candidate,accepted_candidate,index_accepted,cyc
      REAL :: rnd
      LOGICAL :: susceptible_to_infect
      susceptible_to_infect=.FALSE.
    !
    ! CHOOSING A RANDOM INFECTED NODE AS ORIGIN
    ! 
      CALL RANDOM_NUMBER(rnd)
    origin=int(rnd*(n_infected-1))+1
    !Looping for all neighbours of the origin till we find an susceptible node
    IF (D_c(infected(origin)).gt.0) THEN
        DO j=P_ini_c(infected(origin)),P_fin_c(infected(origin))
            candidate=V_c(j)
            DO k=1,n_susceptible
                IF (candidate.eq.susceptible(k)) THEN
                    accepted_candidate=candidate
                    index_accepted=k
                    susceptible_to_infect=.TRUE.
                    exit
                END IF
            END DO
            IF(susceptible_to_infect.eqv..TRUE.) exit
        END DO
    
    END IF
  !
  ! IF RANDOM DON'T WORK, CHECK ALL OTHER INFECTED NODES
  !
    origin=1
      DO WHILE ((susceptible_to_infect.eqv..FALSE.).and.(origin.lt.n_infected-1))
          IF(susceptible_to_infect.eqv..TRUE.) exit
          !Looping for all neighbours of the origin till we find an susceptible node
          IF (D_c(infected(origin)).gt.0) THEN
              DO j=P_ini_c(infected(origin)),P_fin_c(infected(origin))
                  candidate=V_c(j)
                  DO k=1,n_susceptible
                      IF (candidate.eq.susceptible(k)) THEN
                          accepted_candidate=candidate
                          index_accepted=k
                          susceptible_to_infect=.TRUE.
                          exit
                      END IF
                  END DO
                  IF(susceptible_to_infect.eqv..TRUE.) exit
              END DO
          END IF
          origin=origin+1
      END DO

      IF(susceptible_to_infect.eqv..TRUE.) THEN
          n_infected=n_infected+1
        infected(n_infected)=susceptible(index_accepted)
        susceptible(index_accepted)=susceptible(n_susceptible)
        susceptible(n_susceptible)=0
        n_susceptible=n_susceptible-1
    END IF
  END SUBROUTINE

  SUBROUTINE REHAB_NODE()
      INTEGER :: index_accepted
      REAL :: rnd

      CALL RANDOM_NUMBER(rnd)
      index_accepted=int(rnd*(n_infected-1))+1
      n_recovered=n_recovered+1
      recovered(n_recovered)=infected(index_accepted)

    infected(index_accepted)=infected(n_infected)
    infected(n_infected)=0
    n_infected=n_infected-1
   END SUBROUTINE



  FUNCTION denominator()
      REAL :: denominator
      denominator=active_links*r_infect+n_infected*r_rehab
  END FUNCTION
  FUNCTION p_infection()
      REAL :: p_infection
      p_infection=active_links*r_infect/(active_links*r_infect+n_infected*r_rehab)
  END FUNCTION
  SUBROUTINE count_susceptible_links()
      INTEGER :: i,j,k,candidate
      active_links=0
      DO i=1,n_infected
        DO j=P_ini(infected(i)),P_fin(infected(i))
            candidate=V_c(j)
            DO k=1,n_susceptible
                IF (candidate.eq.susceptible(k)) THEN
                    active_links=active_links+1
                END IF
            END DO
        END DO
      END DO
  END SUBROUTINE
  SUBROUTINE count_susceptible_links_confined()
      INTEGER :: i,j,k,candidate
      active_links=0
      DO i=1,n_infected
        DO j=P_ini_c(infected(i)),P_fin_c(infected(i))
            candidate=V_c(j)
            DO k=1,n_susceptible
                IF (candidate.eq.susceptible(k)) THEN
                    active_links=active_links+1
                END IF
            END DO
        END DO
      END DO
  END SUBROUTINE
  
END MODULE
