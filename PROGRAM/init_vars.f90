MODULE INIT_VARS
    use READ_DATA
    IMPLICIT NONE

    CONTAINS
    SUBROUTINE READ_NETWORK1()
        IMPLICIT NONE
        INTEGER :: i,j,line
        node_offset=(1-1*node_offset)
        n_nodes=0
        n_edges=0
        OPEN(12, FILE='network_n.txt')
        DO
            READ(12,*,IOSTAT=line) i,j
            IF(line.gt.0)THEN
                exit
            ELSE IF(j.gt.i) THEN
                n_edges=n_edges+1
                IF (j.gt.n_nodes) THEN
                    n_nodes=j
                END IF
            END IF
        END DO
        close(12)
        n_nodes=n_nodes+node_offset
        
        ALLOCATE(D(n_nodes))
        D=0
        OPEN(12, FILE='network_n.txt')
        DO
            READ(12,*,IOSTAT=line) i,j
            IF(line.gt.0)THEN
                exit
            ELSE IF(j.gt.i) THEN
                D(i+node_offset)=D(i+node_offset)+1
                D(j+node_offset)=D(j+node_offset)+1
            END IF
        END DO
        close(12)
        
        ALLOCATE(V(2*n_edges), P_ini(n_nodes), P_fin(n_nodes))
        V=0;P_ini=0;P_fin=0
        P_ini(1)=1
        DO i=1,n_nodes-1
            P_fin(i)=P_ini(i)+D(i)-1
            P_ini(i+1)=P_fin(i)+1
        END DO
        P_fin(n_nodes)=P_fin(n_nodes-1)+D(n_nodes)
        D=0

        OPEN(12, FILE='network_n.txt')
        DO
            READ(12,*,IOSTAT=line) i,j
            IF(line.gt.0)THEN
                exit
            ELSE IF(j.gt.i) THEN
                V(P_ini(i+node_offset)+D(i+node_offset))=j+node_offset
                V(P_ini(j+node_offset)+D(j+node_offset))=i+node_offset
                D(i+node_offset)=D(i+node_offset)+1
                D(j+node_offset)=D(j+node_offset)+1
            END IF
        END DO
        close(12)
    END SUBROUTINE
    SUBROUTINE READ_NETWORK2()
        IMPLICIT NONE
        INTEGER :: i,j,line
        n_edges_c=0
        OPEN(12, FILE='network_c.txt')
        DO
            READ(12,*,IOSTAT=line) i,j
            IF(line.gt.0)THEN
                exit
            ELSE IF(j.gt.i) THEN
                n_edges_c=n_edges_c+1
            END IF
        END DO
        close(12)
        
        ALLOCATE(D_c(n_nodes))
        D_c=0
        OPEN(12, FILE='network_c.txt')
        DO
            READ(12,*,IOSTAT=line) i,j
            IF(line.gt.0)THEN
                exit
            ELSE IF(j.gt.i) THEN
                D_c(i+node_offset)=D_c(i+node_offset)+1
                D_c(j+node_offset)=D_c(j+node_offset)+1
            END IF
        END DO
        close(12)
        
        ALLOCATE(V_c(2*n_edges), P_ini_c(n_nodes), P_fin_c(n_nodes))
        V_c=0;P_ini_c=0;P_fin_c=0
        P_ini_c(1)=1
        DO i=1,n_nodes-1
            P_fin_c(i)=P_ini_c(i)+D_c(i)-1
            P_ini_c(i+1)=P_fin_c(i)+1
        END DO
        P_fin_c(n_nodes)=P_fin_c(n_nodes-1)+D_c(n_nodes)
        D_c=0

        OPEN(12, FILE='network_n.txt')
        DO
            READ(12,*,IOSTAT=line) i,j
            IF(line.gt.0)THEN
                exit
            ELSE IF(j.gt.i) THEN
                V_c(P_ini_c(i+node_offset)+D_c(i+node_offset))=j+node_offset
                V_c(P_ini_c(j+node_offset)+D_c(j+node_offset))=i+node_offset
                D_c(i+node_offset)=D_c(i+node_offset)+1
                D_c(j+node_offset)=D_c(j+node_offset)+1
            END IF
        END DO
        close(12)
    END SUBROUTINE
END MODULE INIT_VARS