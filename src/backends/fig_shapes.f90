module fig_shapes

    use fig_rgb
    type, abstract :: shape
        type(RGB) :: fill_color
        type(RGB) :: stroke_color
    end type shape

    type, extends(shape) :: circle
        real :: cx, cy, r
    end type circle

    type, extends(shape) :: rectangle
        real :: x, y, width, height
    end type rectangle

    type, extends(shape) :: line
        real :: x1, y1
        real :: x2, y2
    end type line

    type :: shapeWrapper
      class(shape), allocatable :: sh
    end type
end module fig_shapes
