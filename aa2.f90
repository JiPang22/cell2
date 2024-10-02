program aa
integer i, k, imax, kmax, row_index, kk
real * 16 dt,sumIM,sumRE, time_temp, voltage_temp,time_temp2, voltage_temp2
parameter(imax = 50001)
real * 16, dimension(imax) :: time, voltage
real * 16, dimension(6) :: om_ext
character(len = 30) file30_400
character(len = 100) line


om_ext(1) = 0.4
om_ext(2) = 0.7
om_ext(3) = 1.0
om_ext(4) = 1.3
om_ext(5) = 1.6

k = 0,5

filename = './30/C1Trace0000' // k // '.txt'
open(1, file=filename, status='old', action='read', iostat=io_status)
open(2, file='xt30_400', status='unknown', action='write', iostat=io_status)


do i = 1, 50006
read(1,'(A)', end =100) line
end do ! end line pass
open(2, file='xt30_400', status='old', action='read', iostat=io_status)


do
read(1,*,end=100) time_temp, voltage_temp
write(2, '(F10.6, 1X, F10.6)') time_temp, voltage_temp
enddo ! end record


100 continue
    close(1)
    close(2)
open(2, file='xt30_400', status='old', action='read', iostat=io_status)



do i=1,50000
read(2,*, end=100) time_temp2, voltage_temp2
time(i) = time_temp2
voltage(i) = voltage_temp2
end do ! end time series record
close(2)


!reset
sumIM = 0.
sumRE = 0.

open(3, file = 'bb')
dt = 1.e-7
om_ext=
do i = 1, imax
sumRE = sumRE + dt * voltage(i) * cos(om_ext * time(i))
sumIM = sumIM + dt * voltage(i) * sin(om_ext * time(i))
end do ! end sum
write(3, *) om_ext, 2. * sqrt(sumIM**2 + sumRE**2)
enddo ! end 
end program aa