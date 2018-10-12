module rkm_module
      use precision_module
      implicit none
      contains
              subroutine runge_kutta_merson(y, fun, ifail, n, a, b, tol)

                      !runge-kutta-merson method for the solution
                      !of a system of n
                      !1st order initial value ordinary
                      !differential equations.
                      !the routine tries to integrate from t=a to t=b
                      !with initial conditions in y, subject to the
                      !condition that the absolute error estimate
                      ! <= tol. the step length is adjusted
                      ! automatically to meet this condition. if the
                      ! routine is successful it returns with ifail=0,
                      ! t=b and the solution in y.


                      implicit none
                      !define arguments
                      real (long), intent(inout):: y(:)
                      real (long), intent(in):: a,b,tol
                      integer, intent(in)::n
                      integer,intent(out)::ifail

                      interface
                              subroutine fun(t,y,f,n)
                                      use precision_module
                                      implicit none
                                      real (long), intent(in), dimension(:)::y
                                      real (long), intent(out), dimension(:)::f
                                      integer, intent(in)::n
                                      end subroutine fun
                              end interface

                              !local variables

                              real (long), dimension(1:size(y)):: &
                                      s1,s2,s3,s4,s5,new_y_1,new_y_2,error
                              real (long)::t,h,h2,h3,h6,h8,factor=1.e-2_long
                              real (long)::smallest_step=1.e-6_long,max_error
                              integer::no_of_steps=0
                              ifail=0

                              !check input parameters

                              if(n <=0 .or. a==b .or. tol<=0.0) then
                                      ifail = 1
                                      return
                              endif
                              
                              !initialize t to be start of interval and h has to be 1/100 of an
                             ! interval
                              t=a
                              h=(b-a)/100.0_long
                              do !beginning of repeat loop
                              h2=h/2.0_long
                              h3=h/3.0_long
                              h6=h/6.0_long
                              h8=h/8.0_long

                              !calculate s1,s2,s3,s4,s5

                              !s1 = f(t,y)
                              call fun(t,y,s1,n)
                              new_y_1=y+h3*s1

                              !s2 = f(t+h/3, y+h/3*s1)
                              call fun(t+h3,new_y_1,s2,n)
                              new_y_1=y+h6*s1+h6*s2

                              !s3 = f(t+h/3,y+h/6*s1+h/6*s2)
                              call fun(t+h3,new_y_1,s3,n)
                              new_y_1=y+h8*(s2+3.0_long*s3)

                              !s4=f(t+h/2,y+h/8*(s2+3*s3))
                              call fun(t+h2,new_y_1,s4,n)
                              new_y_1=y+h2*(s1-3.0_long*s3+4.0_long*s4)

                              !s5=f(t+h,y+h/2*(s1-3*s3+4*s4))
                              call fun(t+h,new_y_1,s5,n)
                              !calculate values at t+h
                              new_y_1=y+h6*(s1+4.0_long*s4+s5)
                              new_y_2=y+h2*(s1-3.0_long*s3+4.0_long*s4)
                              !calculate error estimates
                              error=abs(0.2_long*(new_y_1-new_y_2))
                              max_error=maxval(error)
                              if(max_error > tol) then
                                      !halve step length and try again
                                      if(abs(h2) < smallest_step) then
                                              ifail=2
                                              return
                                      endif
                                      h=h2
                              else
                                      !accepted approximation so overwrite y with y_new_1
                                      !and t with t+h
                                      y=new_y_1
                                      t=t+h
                                      !can next step be doubled?
                                      if(max_error*factor < tol) then
                                              h=h*2.0_long
                                      endif
                                      !does next step go beyond interval end b, if so set h=b-t
                                      if(t+h > b) then
                                              h=b-t
                                      endif
                                      no_of_steps=no_of_steps+1
                              endif
                              if(t >= b) exit   !end of repeat loop
                              end do
                              end subroutine runge_kutta_merson
                              end module rkm_module
