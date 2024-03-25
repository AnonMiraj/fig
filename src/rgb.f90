module fig_rgb
    implicit none

    type :: RGB
        sequence
        integer :: red
        integer :: green
        integer :: blue
    end type RGB

    type(RGB) :: RED = RGB(255, 0, 0)
    type(RGB) :: GREEN = RGB(0, 255, 0)
    type(RGB) :: BLUE = RGB(0, 0, 255)
    type(RGB) :: WHITE = RGB(255, 255, 255)
    type(RGB) :: BLACK = RGB(0, 0, 0)
    type(RGB) :: YELLOW = RGB(255, 255, 0)
    type(RGB) :: CYAN = RGB(0, 255, 255)
    type(RGB) :: MAGENTA = RGB(255, 0, 255)
    type(RGB) :: PINK = RGB(255, 192, 203)

contains

    function rgb_to_int(color) result(rgb_int)
        type(RGB), intent(in) :: color
        integer :: rgb_int
        rgb_int = color%blue * 65536 + color%green * 256 + color%red
    end function rgb_to_int

end module fig_rgb
