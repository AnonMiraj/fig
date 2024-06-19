module fig_shapes
    use fig_types

    use fig_rgb
    type, abstract :: shape
        type(RGB) :: fill_color
        type(RGB) :: stroke_color
    end type shape

    type, extends(shape) :: circle
        type(point) :: center
        real :: r
    end type circle

    type, extends(shape) :: ellipse
        type(point) :: center
        real :: rx, ry
    end type ellipse

    type, extends(shape) :: rectangle
        type(point) :: upper_left
        real :: width, height
    end type rectangle

    type, extends(shape) :: triangle
        type(point) :: p1, p2, p3
    end type triangle

    type, extends(shape) :: line
        type(point) :: p1,p2
        integer :: stroke_width  =1
    end type line

    type :: shapeWrapper
      class(shape), allocatable :: sh
    end type
end module fig_shapes
