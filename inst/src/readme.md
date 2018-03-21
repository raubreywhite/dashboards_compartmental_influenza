di_edge_list.txt are the number of commuters
pop_wo_com.txt is the number of NON-commuters every day

S E I (IR) R
(IR)= asymptomatisk


Initialization happens:
			if(G_current.locations[40].S > 10){  // Number 40 is Oslo
				G_current.locations[40].I += 10;
				G_current.locations[40].S -= 10;
			}	

main output:
cpp_res_series 
600 lines = 300 days which are updated 2x/day

