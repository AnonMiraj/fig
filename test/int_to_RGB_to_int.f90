program int_to_RGB_to_int
!checks the functions  rgb_to_int and
   use fig_rgb
   type(RGB) :: rgb_old, rgb_new
   integer   :: color_integer
   integer   :: color_counter
   real      :: intermediate_real

   do color_counter = 1, 1000
      call random_number(intermediate_real)
      rgb_old%r = intermediate_real
      call random_number(intermediate_real)
      rgb_old%g = intermediate_real
      call random_number(intermediate_real)
      rgb_old%b = intermediate_real
      call random_number(intermediate_real)
      rgb_old%a = intermediate_real

      color_integer = rgb_to_int(rgb_old)
      rgb_new = int_to_rgb(color_integer)

      if (abs(rgb_old%r - rgb_new%r) > 1.0e-2 .or. &
          abs(rgb_old%g - rgb_new%g) > 1.0e-2 .or. &
          abs(rgb_old%b - rgb_new%b) > 1.0e-2 .or. &
          abs(rgb_old%a - rgb_new%a) > 1.0e-2) then
         error stop "converting between RGB and int or vice versa went wrong"
      end if

   end do

end program int_to_RGB_to_int
