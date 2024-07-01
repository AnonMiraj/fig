module fig_bitmap_rect
    use plutovg
    use fig_shapes
    use fig_canvas
    use fig_bitmap_utils

contains

    subroutine write_rectangle(canva, pluto, rect)
        class(base_canvas), intent(inout) :: canva
        type(c_ptr), intent(inout):: pluto
        type(rectangle), intent(in) :: rect
        type(canvas_point) :: p

        p = to_canvas ( rect%upper_left , canva%size)

        call plutovg_rect(pluto, p%x, p%y, rect%width, rect%height)
        call fill(pluto,rect%fill_color)
        call stroke(pluto,rect%stroke_color, rect%stroke_width)

   
    end subroutine write_rectangle

end module fig_bitmap_rect
