program int_to_RGB_to_int
!checks the functions  rgb_to_int and
  use fig_rgb
  type(RGB) :: rgb_old, rgb_new
  integer   :: color_integer
  integer   :: color_counter
  real      :: intermediate_real

  do color_counter=1,1000
     call random_number(intermediate_real)
     rgb_old%r = int(256*intermediate_real)     
     call random_number(intermediate_real)
     rgb_old%g = int(256*intermediate_real)
     call random_number(intermediate_real)
     rgb_old%b = int(256*intermediate_real)
     call random_number(intermediate_real)
     rgb_old%a = int(256*intermediate_real)

     color_integer = rgb_to_int(rgb_old)

     rgb_new=int_to_rgb(color_integer)

     if (rgb_old%r /= rgb_new%r .or.  &
         rgb_old%g /= rgb_new%g .or.  &
         rgb_old%b /= rgb_new%b .or.  &
         rgb_old%a /= rgb_new%a ) then
        error stop "converting between RGB and int or vice versa went wrong"
     endif
     
  end do

end program int_to_RGB_to_int
  
  
  
