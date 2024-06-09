module fig_types
    use fig_config
    implicit none

    type :: point
        real :: x
        real :: y
    end type point

    type :: canvas_point
        integer(pixel) :: x
        integer(pixel) :: y
    end type canvas_point

    type :: canvas_size
        integer(pixel) :: width
        integer(pixel) :: height
    end type canvas_size

contains

    elemental type(canvas_point) function to_canvas(x, size) result(pxl)
        type(point), intent(in) :: x
        type(canvas_size), intent(in) :: size

        pxl%x = nint(x%x * size%width, kind=pixel)
        pxl%y = nint(x%y * size%height, kind=pixel)
    end function to_canvas

end module fig_types
