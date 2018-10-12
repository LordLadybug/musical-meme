program ch160402
      !chapter 16 exercise to generate truth tables with logicals
      implicit none
      
      logical:: a,b,c,d,e


      write(*,*)'|   x1    |    x2   | .not.x1 |x1.and.x2|x1.or.x2 |'
      a=.true.
      b=a
      c=.not.a
      d=a.and.b
      e=a.or.b
      write(*,fmt=200) a, b, c, d, e

      a=.true.
      b=.not.a
      c=.not.a
      d=a.and.b
      e=a.or.b
      write(*,fmt=200) a, b, c, d, e

      a=.false.
      b=.not.a
      c=.not.a
      d=a.and.b
      e=a.or.b
      write(*,fmt=200) a, b, c, d, e

      a=.false.
      b=a
      c=.not.a
      d=a.and.b
      e=a.or.b
      write(*,fmt=200) a, b, c, d, e
      
200 format(1x,'|',4x,1L1,4x,'|',4x,1L1,4x,'|',4x,1L1,4x,'|',4x,1L1,4x,'|',4x,1L1,4x,'|')
end program ch160402
