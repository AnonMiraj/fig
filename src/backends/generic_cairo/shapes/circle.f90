module fig_cairo_circle
    use cairo
    use fig_shapes
    use fig_canvas
    use fig_cairo_utils

contains

    subroutine write_circle(canva, cr, circ)
        class(base_canvas), intent(inout) :: canva
        type(c_ptr), intent(inout):: cr
        type(circle), intent(in) :: circ
        type(canvas_point) :: c
        real(kind=8) :: left , top , right , bottom , cpx, cpy

        c = to_canvas ( circ%c , canva%size)

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
        call fill(cr,circ)
        call stroke(cr,circ)

    end subroutine write_circle

end module fig_cairo_circle

