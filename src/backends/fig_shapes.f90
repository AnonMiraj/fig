module fig_shapes

    use fig_rgb
    type, abstract :: shape
        type(RGB) :: fill_color
        type(RGB) :: stroke_color

    end type shape

    type, extends(shape) :: circle
        real :: cx, cy, r
    end type circle

    type, extends(shape) :: ellipse
        real :: cx, cy, rx,ry
    end type ellipse

    type, extends(shape) :: rectangle
        real :: x, y, width, height
    end type rectangle

    type :: shapeWrapper
      class(shape), allocatable :: sh
    end type
end module fig_shapes
