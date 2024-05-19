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

    type(RGB) :: RED = RGB(255, 0, 0, 255) 
    type(RGB) :: GREEN = RGB(0, 255, 0, 255)
    type(RGB) :: BLUE = RGB(0, 0, 255, 255)
    type(RGB) :: WHITE = RGB(255, 255, 255, 255)
    type(RGB) :: BLACK = RGB(0, 0, 0, 255)
    type(RGB) :: YELLOW = RGB(255, 255, 0, 255)
    type(RGB) :: CYAN = RGB(0, 255, 255, 255)
    type(RGB) :: MAGENTA = RGB(255, 0, 255, 255)
    type(RGB) :: PINK = RGB(255, 192, 203, 255)
    type(RGB) :: ORANGE = RGB(255, 165, 0, 255)
    type(RGB) :: PURPLE = RGB(128, 0, 128, 255)

contains

    function rgb_to_int(color) result(rgb_int)
        type(RGB), intent(in) :: color
        integer(pixel) :: rgb_int
      
        rgb_int = ior(ishft(color%a, rgb_bit_depth*3),&
                  ior(ishft(color%b, rgb_bit_depth*2),&
                  ior(ishft(color%g, rgb_bit_depth), color%r)))

     end function rgb_to_int
      
     function int_to_rgb(rgb_int) result(color)
       type(RGB):: color
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
         write(color_string, '(A,I3,A,I3,A,I3,A,F5.3,A)') 'rgba(', color%r, ',', color%g, ',', color%b, ',', alpha, ')'
     end function rgb_to_string     

end module fig_rgb
