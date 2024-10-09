program aaa
IMPLICIT NONE
integer i
real, parameter :: tmax = 100., dt = 1.e-2
integer, parameter :: imax = int(tmax/dt)
real x_dash, dx_dash, random_num, t, x, y, dx, dy, z1, z2, u1, u2
real, dimension(imax) :: xt, noise
real, parameter :: eta = 1., gam = 0.1
real, parameter :: tau_a = 0.1
!real, parameter :: tau_a = 25.


open(1, file = 'xt-curve') !>> make file!
open(2, file = 'noise-curve') !>> make file!


!>>  make noise
call random_seed()  

do i = 1, imax / 2
write(2, *) t, noise(i)

call random_number(u1)
call random_number(u2)

z1 = sqrt(-2. * log(u1)) * cos(2. * 3.14 * u2)
z2 = sqrt(-2. * log(u1)) * sin(2. * 3.14 * u2)

noise(2 * i - 1) = z1 * (1./3.6) * 1.e-2
noise(2 * i) = z2 * (1./3.6) * 1.e-2

end do  !>>     end noise make


t = 0.

do i = 1, imax
write(2, *) t, noise(i)
t = t + dt

end do 



!>> initial conditions
t = 0.
x = 7.
x_dash = 0.
y = 0.

do i = 1, imax !>> i is time index
xt(i) = x  !>>> recode x(t)
write(1, *) t, x

dy = - gam * y - x + noise(i) + (1. / 2.) * eta * sign(1., x - x_dash)
dx = y
dx_dash = (x - x_dash) / tau_a

y = y + dy * dt
x = x + dx * dt
x_dash = x_dash + dx_dash * dt 
t = t + dt

end do   !>> i end // !>> fixed om_ext, recode xt!
end program