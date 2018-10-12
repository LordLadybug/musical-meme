program ch2502
      use precision_module
      use rkm_module
      use fun1_module
      implicit none
      
      real (long),dimension(:),allocatable::y
      real (long)::a,b,tol
      integer::n,ifail,all_stat
      
      print *, 'input no of equations'
      read *, n

      !allocate space for y - checking to see that it allocates properly
      allocate(y(1:n), stat=all_stat)
      if(all_stat /= 0) then
              print *, 'not enough memory'
              print *, 'array y is not allocated'
              stop
      endif
      print *, 'input start and end of interval over'
      print *,' which equations to be solved'
      read *,a,b
      print *, "input initial conditions"
      read *,y(1:n)
      print *, 'input tolerance'
      read *, tol
      print 100,a
      100 format('at t=', f5.2,' initial conditions are :')
      print 200,y(1:n)
      200 format(4(f5.2,2x))
      call runge_kutta_merson(y,fun1,ifail,n,a,b,tol)
      if(ifail /= 0) then
              print *, 'integration stopped with ifail = ',ifail
      else
              print 300,b
              300 format('at t= ',f5.2,' solution is: ')
              print 200,y(1:n)
      endif
      end program ch2502
