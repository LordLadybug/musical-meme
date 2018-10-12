module fun1_module

      implicit none
      contains
              subroutine fun1(t,y,f,n)
                      use precision_module
                      implicit none
                      real (long), intent(in), dimension(:)::y
                      real (long), intent(out), dimension(:)::f
                      real (long), intent(in)::t
                      integer, intent(in)::n

                      f(1)=tan(y(3))
                      f(2)=-0.032_long*f(1)/y(2)-0.02_long*y(2)/cos(y(3))
                      f(3)=-0.032_long/(y(2)*y(2))
              end subroutine fun1
 end module fun1_module
