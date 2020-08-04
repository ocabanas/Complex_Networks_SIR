MODULE CONFIG
    use READ_DATA
    IMPLICIT NONE

    CONTAINS
    SUBROUTINE PRINT_INITIAL()
        IMPLICIT NONE
        INTEGER :: i,j,max_val,k,triangles,lk_1,lk_2,lk_3
        REAL*8 :: avg_neigh,c_i

        
        print*,'number of nodes',n_nodes
        WRITE(20,*)'number of nodes',n_nodes
        print*,'number of edges',n_edges
        WRITE(20,*)'number of edges',n_edges
        print*,'average degree',real(sum(D))/real(n_nodes)
        WRITE(20,*)'average degree',real(sum(D))/real(n_nodes)


        WRITE(21,*)'#Node----Degree'
        DO i=1,n_nodes
        	WRITE(21,*)i,D(i)
        END DO


        max_val=maxval(D)
        
        ALLOCATE(D_dist(max_val+1), D_cumul(max_val+1))
        ALLOCATE(k_nn(max_val+1),c(max_val+1))
        D_dist=0
        D_cumul=0
        DO i=1,n_nodes
        	D_dist(D(i))=D_dist(D(i))+1
        END DO
        DO i=1,max_val
        	DO j=i,max_val
        		D_cumul(i)=D_cumul(i)+D_dist(j)
        	END DO
        END DO
        print*,'Node distribution done'

        DO i=1,n_nodes
        	avg_neigh=0.
        	k=D(i)
        	DO j=P_ini(i),P_fin(i)
        		avg_neigh=avg_neigh+D(V(j))
        	END DO
        	k_nn(k)=k_nn(k)+avg_neigh/(real(k*D_dist(k)))
        END DO

        print*,'K_nn distribution done'

        DO i=1,n_nodes
        	k=D(i)
        	triangles=0
        	DO lk_1= P_ini(i),P_fin(i)
        		DO lk_2= P_ini(V(lk_1)),P_fin(V(lk_1))
        			DO lk_3= P_ini(V(lk_2)),P_fin(V(lk_2))
	        			IF (lk_3.eq.i) THEN
	        				triangles=triangles+1
	        			END IF
	        		END DO
	        	END DO
	        END DO
	        c_i=2.*triangles/(1.*k*D_dist(k)*(k-1.))
	        c(k)=c(k)+c_i
	    END DO

	    print*,'clust coeff distribution done'

        DO i=1,max_val
        	WRITE(22,*)i,real(D_dist(i))/real(n_nodes),real(D_cumul(i))/real(n_nodes),k_nn(i),c(i)
        END DO
    END SUBROUTINE

END MODULE
