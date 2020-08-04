reset
set term png

set output 'fig_deg_list.png'
plot 'degree_list.dat' u 1:2 w p

set output 'fig_deg_distrib.png'
plot 'degree_distrib.dat' u 1:2 w p

set output 'fig_deg_cumul.png'
plot 'degree_distrib.dat' u 1:3 w p

set output 'fig_k_nn.png'
plot 'degree_distrib.dat' u 1:4 w p

set output 'fig_c_coeff.png'
plot 'degree_distrib.dat' u 1:5 w p