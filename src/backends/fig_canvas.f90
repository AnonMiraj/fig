module fig_canvas
    use fig_config
    use fig_shapes
    implicit none
    
    type, abstract :: base_canvas 
        real :: width, height
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

    subroutine init(this, width, height)
        class(base_canvas), intent(inout) :: this
        real, intent(in) :: width, height

        this%width = width
        this%height = height

    end subroutine init

end module fig_canvas
