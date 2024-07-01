module fig_bitmap_ellipse
    use plutovg
    use fig_shapes
    use fig_canvas
    use fig_bitmap_utils

contains

    subroutine write_ellipse(canva, pluto, ellip)
        class(base_canvas), intent(inout) :: canva
        type(c_ptr), intent(inout):: pluto
        type(ellipse), intent(in) :: ellip
        type(canvas_point) :: c

        c = to_canvas ( ellip%center , canva%size)

        call plutovg_ellipse(pluto, c%x, c%y, ellip%rx,ellip%ry)
        call set_rgba(pluto,ellip%fill_color)
        call plutovg_fill_preserve(pluto)
        call set_rgba(pluto,ellip%stroke_color)
        call plutovg_set_line_width(pluto, ellip%stroke_width)
        call plutovg_stroke(pluto)

    end subroutine write_ellipse

end module fig_bitmap_ellipse
