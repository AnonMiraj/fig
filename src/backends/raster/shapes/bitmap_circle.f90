module fig_bitmap_circle
    use plutovg
    use fig_shapes
    use fig_canvas
    use fig_bitmap_utils

contains

    subroutine write_circle(canva, pluto, circ)
        class(base_canvas), intent(inout) :: canva
        type(c_ptr), intent(inout):: pluto
        type(circle), intent(in) :: circ
        type(canvas_point) :: c
        c = to_canvas ( circ%center , canva%size)
        call plutovg_circle(pluto, c%x, c%y, circ%r)
        call set_rgba(pluto,circ%fill_color)
        call plutovg_fill_preserve(pluto)
        call set_rgba(pluto,circ%stroke_color)
        call plutovg_set_line_width(pluto, circ%stroke_width)
        call plutovg_stroke(pluto)

    end subroutine write_circle

end module fig_bitmap_circle

