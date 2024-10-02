program aa
file30 = 
do k = 1, kmax
do i = 1, imax
t = dt * i
om_ext = dom_ext * k
sumRE = sumRE + dt * xt(i) * cos(om_ext * t)
sumIM = sumIM + dt * xt(i) * sin(om_ext * t)
end do ! end sum
write(2,*) om_ext, 2. * sqrt(sumIM**2 + sumRE)
end do