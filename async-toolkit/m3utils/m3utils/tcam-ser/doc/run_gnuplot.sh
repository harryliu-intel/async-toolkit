cd $1
gnuplot <<EOF

set term post color

set output "sparse.eps"
set xlabel "rho"
set ylabel "xi"
set logscale x
set logscale y
plot [1e-6:1][0.001:2](1-(1-x)**8192)

set output "worstxi.eps"
plot [1e-6:1][0.001:1e6](x**-(1.0/1)-1)*(1-(1-x)**(2**13)),(x**-(1.0/1)-1),(1-(1-x)**(2**13))

set output "xi10.eps"
plot [1e-6:1][0.001:1e1](x**-(1.0/10)-1)*(1-(1-x)**(2**13)),(x**-(1.0/10)-1),(1-(1-x)**(2**13))

set output "xi13.eps"
plot [1e-6:1][0.001:1e1](x**-(1.0/13)-1)*(1-(1-x)**(2**13)),(x**-(1.0/13)-1),(1-(1-x)**(2**13))

set output "partition_tcam.eps"
set xlabel "N"
set ylabel "xi"
plot [1:32768] ((1.0/x)**-(1.0/10.0)-1.0)*(1.0-(1.0-1.0/x)**x),((1.0/x)**-(1.0/40.0)-1.0)*(1.0-(1.0-1.0/x)**x),1



EOF
