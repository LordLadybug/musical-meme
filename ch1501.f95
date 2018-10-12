program ch1501
     implicit none


      real :: Omega, real_part, Imag_part, magnitude, phase
      complex:: Frequency_response
      print *, 'Omega :'
      read *, Omega

      Frequency_response = 1.0/ &
             cmplx(-Omega*Omega+1.0, 2.0 * Omega)

      real_part = real(Frequency_response)
      Imag_part = aimag(Frequency_response)
      ! a+bi --> polar coordinates
      magnitude = abs(Frequency_response)
      phase = atan2(Imag_part, real_part)

      print *, 'at frequency', Omega
      print *, 'response = ', real_part, '+ I', Imag_part
      print *, 'In polar form: '
      print *, 'magnitude = ', magnitude
      print *, 'phase = ', phase

end program ch1501
