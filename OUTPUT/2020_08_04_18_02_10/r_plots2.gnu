reset
set term png

set output 'fig_dynamics1.png'
plot 'dynamics.dat' u 1:3 w p t"active", '' u 1:4 w p t"infected", "" u 1:5 w p t"recovered"
set output 'fig_dynamics2.png'
plot 'dynamics.dat' u 2:3 w p t"active", '' u 2:4 w p t"infected", "" u 2:5 w p t"recovered"