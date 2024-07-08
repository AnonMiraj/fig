module fig_cairo_utils
    use fig_canvas
    use fig_rgb
    use cairo
    use fig_config
    implicit none
    real(kind=8),parameter :: kappa = 0.55228474983079339840
    
contains
    
    subroutine set_rgba(cr,color)
        type(c_ptr), intent(inout) :: cr
        type(RGB) :: color
         call cairo_set_source_rgba(cr,normalize_ch(color%r),normalize_ch(color%g),normalize_ch(color%b),normalize_ch(color%a))
    end subroutine set_rgba

    function normalize_ch(ch) result(res)
        integer, intent(in) :: ch
        real(kind=8) :: res
        res= real(ch,kind=8)/real(2**rgb_bit_depth-1,kind=8)
    end function normalize_ch

    subroutine fill(cr,sh)
        type(c_ptr), intent(inout) :: cr
        class(shape), intent(in) :: sh
        if (sh%fill_color%a .ne. 0) then
            call set_rgba(cr,sh%fill_color)
            call cairo_fill_preserve(cr)
        end if               
    end subroutine fill

    subroutine stroke(cr,sh)
        type(c_ptr), intent(inout) :: cr
        class(shape), intent(in) :: sh

        if (sh%stroke_color%a .ne. 0) then
            call set_rgba(cr,sh%stroke_color)
            call cairo_set_line_width(cr,sh%stroke_width)
            call cairo_stroke(cr)
        else 
            call cairo_new_path(cr)
        end if

    end subroutine stroke

    subroutine quad_to(cr, cx, cy, x1, y1, x2, y2)
        type(c_ptr), intent(inout) :: cr
        real(kind=8),intent(in) :: cx, cy, x1, y1, x2, y2
        real(kind=8):: cx1, cy1
        real(kind=8):: cx2, cy2

        cx1 = 2.0 / 3.0 * x1 + 1.0 / 3.0 * cx
        cy1 = 2.0 / 3.0 * y1 + 1.0 / 3.0 * cy
        cx2 = 2.0 / 3.0 * x1 + 1.0 / 3.0 * x2
        cy2 = 2.0 / 3.0 * y1 + 1.0 / 3.0 * y2
        call cairo_curve_to(cr,cx1, cy1, cx2, cy2, x2, y2)

    end subroutine quad_to

end module fig_cairo_utils
