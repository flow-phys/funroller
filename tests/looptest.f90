program looptest

  implicit none
  integer :: n,iter
  integer :: t,i,j,k
  double precision, dimension(:,:,:), allocatable :: data,tmp

  ! Timers vars
  real :: start, finish
  !$DEF
  
  n = 32
  iter = 10000
  

  allocate(data(n,n,n))
  allocate(tmp(n,n,n))
  tmp = 3.2
  
  call cpu_time(start)


  
  do t=1,iter/2

     !$F2GPU
     !$UNROLL {dim:3,var:['data','tmp'] }
     data = tmp - .2D0
     tmp = data / 3.0D0
     data = exp(tmp)*0.0 + 1.0 + dble(t)
     !$END UNROLL
     !$END RAFA
     
  end do
  
  do t=1,iter/2

     !$RAFA 
     !$UNROLL {dim:3,var:['data','tmp'] }
     data = tmp - .2D0
     tmp = data / 3.0D0
     data = exp(tmp)*0.0 + 1.0 + dble(t)
     !$END UNROLL
     !$END RAFA
     
  end do

  call cpu_time(finish)
  print '("Intrinsic: Time = ",e14.7," seconds.")',finish-start
  print '("Sum:",e14.7)',sum(data)


  data = 0.0
  
  call cpu_time(start)
  do t=1,iter
     call compute_stuff(data,tmp,t,n)
  end do
  call cpu_time(finish)
  print '("Function: Time = ",e14.7," seconds.")',finish-start
  

  print '("Sum:",e14.7)',sum(data)

  

  call cpu_time(start)
  
  do t=1,iter
     !$OMP PARALLEL DO collapse(3)
     do k=1,n
        do j=1,n
           do i=1,n
              data(i,j,k) = tmp(i,j,k) - .2D0
              tmp(i,j,k) = data(i,j,k) / 3.0D0
              data(i,j,k) = exp(tmp(i,j,k))*0.0 + 1.0 + dble(t)
           end do
        end do
     end do
     !$OMP END PARALLEL DO
  end do

  call cpu_time(finish)
  print '("Unrolled: Time = ",e14.7," seconds.")',finish-start
  print '("Sum:",e14.7)',sum(data)
  

end program looptest


subroutine compute_stuff(A,B,t,n)

  implicit none
  integer :: t,n
  double precision, dimension(n,n,n), intent(inout) :: A,B
  !$DEF

  !$UNROLL {dim:3,var:['A','B']}
  A = B - .2D0
  B = A / 3.0D0
  A = exp(B)*0.0 + 1.0 + dble(t)
  !$END UNROLL
  
end subroutine compute_stuff
