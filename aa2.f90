program aa
integer i, k, imax, kmax, row_index
real * 16 dt,sumIM,sumRE, time_temp, voltage_temp,time_temp2, voltage_temp2, om_ext
parameter(imax = 50001)
real * 16, dimension(imax) :: time, voltage
character(len = 30) file30_400
character(len = 100) line


file30_400 = './30/C1Trace00000.txt'

open(1, file=file30_400, status='old', action='read', iostat=io_status)




open(2, file='xt30_400', status='unknown', action='write', iostat=io_status)




do i = 1, 50006
read(1,'(A)', end =100) line
end do


open(2, file='xt30_400', status='old', action='read', iostat=io_status)


do
read(1,*,end=100) time_temp, voltage_temp
write(2, '(F10.6, 1X, F10.6)') time_temp, voltage_temp
enddo


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


sumIM = 0.
sumRE = 0.

open(3, file = 'bb')
dt = 1.e-7

om_ext=400.
do i = 1, imax

sumRE = sumRE + dt * voltage(i) * cos(om_ext * time(i))
sumIM = sumIM + dt * voltage(i) * sin(om_ext * time(i))


end do ! end sum
write(3, *) om_ext, 2. * sqrt(sumIM**2 + sumRE**2)
write(*, *) om_ext, 2. * sqrt(sumIM**2 + sumRE**2)
end program aa