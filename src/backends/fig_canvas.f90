module fig_canvas
    use fig_config
    use fig_shapes
    use fig_types
    implicit none
    
    type, abstract :: base_canvas 
        type(canvas_size) :: size
        character(len=:), allocatable :: title
    contains
        procedure(canvas_draw_shape), deferred :: draw_shape
        procedure :: init
    end type base_canvas

    abstract interface
       subroutine canvas_draw_shape(canva,sh)
       import base_canvas, shape 
       class(base_canvas), intent(inout) :: canva
       class(shape), intent(in) :: sh
       end subroutine canvas_draw_shape
    end interface

contains

    subroutine init(this, width, height, title)
        class(base_canvas), intent(inout) :: this
        integer, intent(in) :: width, height
        character(len=*), intent(in) :: title

        this%size%width = width
        this%size%height = height
        this%title=title

    end subroutine init

end module fig_canvas
