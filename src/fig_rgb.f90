module fig_rgb
   use fig_config
   implicit none
   public

   type :: RGB
      sequence
      real(kind=8) :: r
      real(kind=8) :: g
      real(kind=8) :: b
      real(kind=8) :: a
   end type RGB

contains

   elemental type(integer) function rgb_to_int(color) result(rgb_int)
      type(RGB), intent(in) :: color
      integer :: r, g, b, a
      r = color%r*(2**rgb_bit_depth - 1)
      b = color%b*(2**rgb_bit_depth - 1)
      g = color%g*(2**rgb_bit_depth - 1)
      a = color%a*(2**rgb_bit_depth - 1)

      rgb_int = ior(ishft(a, rgb_bit_depth*3), &
                    ior(ishft(r, rgb_bit_depth*2), &
                        ior(ishft(g, rgb_bit_depth), b)))

   end function rgb_to_int

   elemental type(RGB) function int_to_rgb(rgb_int) result(color)
      integer, intent(in):: rgb_int

      color%a = real(ibits(rgb_int, 3*rgb_bit_depth, rgb_bit_depth), kind=8)/(2**rgb_bit_depth - 1)
      color%r = real(ibits(rgb_int, 2*rgb_bit_depth, rgb_bit_depth), kind=8)/(2**rgb_bit_depth - 1)
      color%g = real(ibits(rgb_int, rgb_bit_depth, rgb_bit_depth), kind=8)/(2**rgb_bit_depth - 1)
      color%b = real(ibits(rgb_int, 0, rgb_bit_depth), kind=8)/(2**rgb_bit_depth - 1)
   end function int_to_rgb

end module fig_rgb
