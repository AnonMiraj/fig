module fig_rgb
    use fig_config
    implicit none

    type :: RGB
        sequence
        integer(rgb_level) :: r
        integer(rgb_level) :: g
        integer(rgb_level) :: b
        integer(rgb_level) :: a
    end type RGB

contains

    elemental type(integer(pixel)) function rgb_to_int(color) result(rgb_int)
        type(RGB), intent(in) :: color

        rgb_int = ior(ishft(color%a, rgb_bit_depth*3),&
                  ior(ishft(color%b, rgb_bit_depth*2),&
                  ior(ishft(color%g, rgb_bit_depth), color%r)))

     end function rgb_to_int
      
     elemental type(RGB) function int_to_rgb(rgb_int) result(color)
       integer(pixel), intent(in):: rgb_int

       color%a = ibits(rgb_int, 3*rgb_bit_depth, rgb_bit_depth)
       color%b = ibits(rgb_int, 2*rgb_bit_depth, rgb_bit_depth)
       color%g = ibits(rgb_int, rgb_bit_depth  , rgb_bit_depth)
       color%r = ibits(rgb_int, 0, rgb_bit_depth)       
     end function int_to_rgb

     function rgb_to_string(color) result(color_string)
         type(RGB), intent(in) :: color
         character(len=50) :: color_string
         real :: alpha

         alpha = color%a / 255.0
         alpha = min(1.0,alpha)
         write(color_string, '(A,I0,A,I0,A,I0,A,F5.3,A)') 'rgba(', color%r, ',', color%g, ',', color%b, ',', alpha, ')'
     end function rgb_to_string     

     elemental type(integer(pixel)) function blend_color(c1, c2) result(blended)
        integer(pixel), intent(in) :: c1, c2

        type(RGB) :: color1, color2

        color1 = int_to_rgb(c1)
        color2 = int_to_rgb(c2)

        color1%r = (color1%r * (255 - color2%a) + color2%r * color2%a) / 255
        color1%g = (color1%g * (255 - color2%a) + color2%g * color2%a) / 255
        color1%b = (color1%b * (255 - color2%a) + color2%b * color2%a) / 255

        color1%r = min(255, max(0, color1%r))
        color1%g = min(255, max(0, color1%g))
        color1%b = min(255, max(0, color1%b))

        blended = rgb_to_int(color1)
    end function blend_color

end module fig_rgb
