program aa
integer i, k, imax, kmax, row_index, kk
real * 16 dt,sumIM,sumRE, time_temp, voltage_temp,time_temp2, voltage_temp2
parameter(imax = 50001)
real * 16, dimension(imax) :: time, voltage
real * 16, dimension(6) :: om_ext
character(len = 100) filename
character(len = 100) line
character(len=1)  str_num


 ! 출력 파일 열기
    open(2, file='xt30', status='unknown', action='write')
    open(3, file='bb', status='unknown', action='write')

om_ext(1) = 0.4
om_ext(2) = 0.7
om_ext(3) = 1.0
om_ext(4) = 1.3
om_ext(5) = 1.6
om_ext(6) = 1.9



do k = 0,5 
    kk = k + 1

    write(str_num, '(I1)') k
    filename = './30/C1Trace0000' // trim(adjustl(str_num)) // '.txt'

    open(1, file=trim(filename), status='old', action='read')
    
    do i = 1, 50006 !50006까지 건너뛰기
     read(1,'(A)', end =100) line
    end do ! end line pass


    open(2, file='xt30', status='unknown', action='write')
    


    do
     read(1, *, end = 100) time_temp, voltage_temp
     write(2, '(F10.6, 1X, F10.6)') time_temp, voltage_temp
    enddo ! end record

    close(1)
    close(2)

    
    open(2, file='xt30', status='old', action='read')
    
        

    do i=1,50000
     read(2,*, end=100) time_temp2, voltage_temp2
     time(i) = time_temp2
     voltage(i) = voltage_temp2
    end do ! end time series record
    close(2)  ! xt30 파일 닫기 (읽기 완료)
   


    !reset
    sumIM = 0.
    sumRE = 0.

    
    dt = 1.e-7
    do i = 1, imax
     sumRE = sumRE + dt * voltage(i) * cos(om_ext(kk) * time(i))
     sumIM = sumIM + dt * voltage(i) * sin(om_ext(kk) * time(i))
    end do ! end sum

    write(3, *) om_ext(kk), 2. * sqrt(sumIM**2 + sumRE**2)
   ! write(*, *) om_ext(kk), 2. * sqrt(sumIM**2 + sumRE**2)
   close(3)
enddo ! end 

close(2)
close(3)
end program aa