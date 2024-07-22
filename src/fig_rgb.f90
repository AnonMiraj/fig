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
                  ior(ishft(color%r, rgb_bit_depth*2),&
                  ior(ishft(color%g, rgb_bit_depth), color%b)))

     end function rgb_to_int
      
     elemental type(RGB) function int_to_rgb(rgb_int) result(color)
       integer(pixel), intent(in):: rgb_int

       color%a = ibits(rgb_int, 3*rgb_bit_depth, rgb_bit_depth)
       color%r = ibits(rgb_int, 2*rgb_bit_depth, rgb_bit_depth)
       color%g = ibits(rgb_int, rgb_bit_depth  , rgb_bit_depth)
       color%b = ibits(rgb_int, 0, rgb_bit_depth)
     end function int_to_rgb

end module fig_rgb
