module fig_shapes

    use fig_rgb
    type, abstract :: shape
        type(RGB) :: fill_color
    end type shape

    type, extends(shape) :: circle
        real :: cx, cy, r
    end type circle

    type, extends(shape) :: rectangle
        real :: x, y, width, height
    end type rectangle

end module fig_shapes
