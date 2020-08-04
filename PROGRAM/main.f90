PROGRAM SIR
    !---------------------------------------------
    !      IMPORTING ALL MODULES
    !---------------------------------------------
    use READ_DATA
    use INIT_VARS
    use CONFIG
    use thermodynamic_routines
    IMPLICIT NONE
    INTEGER :: index_time,index_cycle,index_instance,n_samples
    REAL :: rnd,time,step,avg_rho
    print*, 'open files'
    OPEN(20,FILE='log.dat')
    OPEN(21,FILE='degree_list.dat')
    OPEN(22, FILE='degree_distrib.dat')

    !*********************************************
    !    READING THE FILES AND CREATING VARIABLES
    !*********************************************
    print*, 'reading input'
    CALL READ_FROM_FILES()
    print*, 'setting networks'
    CALL READ_NETWORK1() !Reading inputs ans setting global variables (module READ_DATA)
    CALL READ_NETWORK2()
    CALL PRINT_INITIAL()  !Allocating and comuting variables for main program ( module INIT_VARS)

    !*********************************************
    !    NETWORK DYNAMICS
    !*********************************************

    CALL RANDOM_SEED()
        print*, 'setting initial infected nodes'
    CALL INITIAL_INFECTED()

    ! STARTING DYNAMICS
    print*,'*****************************'
    print*,'STARTING DYNAMICS'
    print*,'*****************************'
    avg_rho=0.
    n_samples=0
    OPEN(30, FILE='dynamics.dat')
    n_samples=0
    time=0.
    DO index_cycle =1,n_cycles
        print*,'cycle',index_cycle
        !
        ! NO CONFINEMENT
        !
        r_infect=r_infect_origin
        DO index_time=1,n_steps_conf*3
            CALL count_susceptible_links()
            IF (n_infected.eq.0) exit
            CALL RANDOM_NUMBER(rnd)
            step= - log(rnd)/denominator()
            time=time+step
            IF ((rnd.le.p_infection()).and.(n_susceptible.gt.0)) THEN
                CALL INFECT_NODE_N()
            ELSE
                CALL REHAB_NODE()
            END IF
            WRITE(30,*)time,n_susceptible,n_infected,n_recovered
            avg_rho=avg_rho+n_infected
            n_samples=n_samples+1
            IF (n_infected.eq.0) exit
        END DO
        IF (n_infected.eq.0) exit

        !
        ! CONFINEMENT
        !
        IF(is_confinement.eqv..TRUE.) THEN
            r_infect=r_infect_origin*0.1

            DO index_time=1,n_steps_conf*1
                CALL count_susceptible_links_confined()
                IF (n_infected.eq.0) exit
                CALL RANDOM_NUMBER(rnd)
                step= - log(rnd)/denominator()
                time=time+step
                IF ((rnd.le.p_infection()).and.(n_susceptible.gt.0)) THEN
                    CALL INFECT_NODE_C(index_cycle)
                ELSE
                    CALL REHAB_NODE()
                END IF
                WRITE(30,*)time,n_susceptible,n_infected,n_recovered
                avg_rho=avg_rho+n_infected
                n_samples=n_samples+1
                IF (n_infected.eq.0) exit
            END DO
        END IF
        IF (n_infected.eq.0) exit
        CALL EXECUTE_COMMAND_LINE("gnuplot r_plots2.gnu")
    END DO

    avg_rho=avg_rho/real(n_samples*n_nodes)
    print*,'rho',avg_rho
    print*,'lambda',r_infect_origin



    CALL EXECUTE_COMMAND_LINE("gnuplot r_plots1.gnu")
    CALL EXECUTE_COMMAND_LINE("gnuplot r_plots2.gnu")
    
END PROGRAM SIR