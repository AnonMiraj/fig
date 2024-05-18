module fig_shapes

    type, abstract :: shape
        character(len=:), allocatable :: fill_color
    end type shape

    type, extends(shape) :: circle
        real :: cx, cy, r
    end type circle

    type, extends(shape) :: rectangle
        real :: x, y, width, height
    end type rectangle

end module fig_shapes
