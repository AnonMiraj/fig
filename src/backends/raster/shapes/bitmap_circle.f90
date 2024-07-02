module fig_bitmap_circle
    use cairo
    use fig_shapes
    use fig_canvas
    use fig_bitmap_utils

contains

    subroutine write_circle(canva, cr, circ)
        class(base_canvas), intent(inout) :: canva
        type(c_ptr), intent(inout):: cr
        type(circle), intent(in) :: circ
        type(canvas_point) :: c
        real(kind=8) :: left , top , right , bottom , kappa, cpx,cpy

        c = to_canvas ( circ%center , canva%size)

        kappa = 0.55228474983079339840
        left = c%x - circ%r;
        top = c%y - circ%r;
        right = c%x + circ%r;
        bottom = c%y + circ%r;
        
        cpx = circ%r * kappa;
        cpy = circ%r * kappa;


        call cairo_move_to(cr, c%x, top)
        call cairo_curve_to(cr, c%x + cpx, top, right, c%y - cpy, right, c%y);
        call cairo_curve_to(cr, right, c%y + cpy, c%x + cpx, bottom, c%x, bottom);
        call cairo_curve_to(cr, c%x - cpx, bottom, left, c%y + cpy, left, c%y);
        call cairo_curve_to(cr, left, c%y - cpy, c%x - cpx, top, c%x, top);
        call cairo_close_path(cr);
        call fill(cr,circ%fill_color)
        call stroke(cr,circ%stroke_color,circ%stroke_width)

    end subroutine write_circle

end module fig_bitmap_circle

