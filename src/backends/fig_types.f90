module fig_types
    use fig_config
    implicit none

    type :: point
        real :: x
        real :: y
    end type point

    type :: canvas_point
        real(kind=8) :: x
        real(kind=8) :: y
    end type canvas_point

    type :: canvas_size
        integer(pixel) :: width
        integer(pixel) :: height
    end type canvas_size

contains

    elemental type(canvas_point) function to_canvas(p, sz) result(pxl)
        type(point), intent(in) :: p
        type(canvas_size), intent(in) :: sz

        pxl%x = p%x * sz%width
        pxl%y = p%y * sz%height
    end function to_canvas

end module fig_types
