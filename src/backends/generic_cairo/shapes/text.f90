module fig_cairo_text
    use cairo
    use fig_shapes
    use fig_canvas
    use fig_cairo_utils

contains

    subroutine write_text(canva, cr, t)
        class(base_canvas), intent(inout) :: canva
        type(c_ptr), intent(inout):: cr
        type(text), intent(in) :: t
        type(canvas_point) :: p
        
        p= to_canvas(t%p,canva%size)
        
        call cairo_select_font_face(cr, t%font_family//c_null_char, t%slant, t%weight)
        call cairo_set_font_size(cr,t%size)
        call cairo_move_to(cr,p%x,p%y)
        call cairo_text_path(cr,t%content// c_null_char)
        call fill(cr,t)
        call stroke(cr,t)
    end subroutine write_text
    

end module fig_cairo_text
