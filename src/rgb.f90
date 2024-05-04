
module fig_rgb
    implicit none

    type :: RGB
        sequence
        integer :: r
        integer :: g
        integer :: b
        integer :: a
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
        integer :: rgb_int
      
        rgb_int = ior(ishft(color%a, 24), ior(ishft(color%b, 16), ior(ishft(color%g, 8), color%r)))

     end function rgb_to_int
      
     function int_to_rgb(rgb_int) result(color)
       type(RGB):: color
       integer, intent(in):: rgb_int
       color%a = ibits(rgb_int, 24, 8)
       color%b = ibits(rgb_int, 16, 8)
       color%g = ibits(rgb_int, 8, 8)
       color%r = ibits(rgb_int, 0, 8)       
     end function int_to_rgb
      

end module fig_rgb

