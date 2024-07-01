module fig_bitmap_line
    use plutovg
    use fig_shapes
    use fig_canvas
    use fig_bitmap_utils

contains

    subroutine write_line(canva, pluto, l)
        class(base_canvas), intent(inout) :: canva
        type(c_ptr), intent(inout):: pluto
        type(line), intent(in) :: l
        type(canvas_point) :: p1,p2
        
        p1= to_canvas(l%p1,canva%size)
        p2= to_canvas(l%p2,canva%size)

        call plutovg_new_path(pluto)
        call plutovg_move_to(pluto,p1%x,p1%y)
        call plutovg_line_to(pluto,p2%x,p2%y)
        call plutovg_close_path(pluto)
        call set_rgba(pluto,l%stroke_color)
        call plutovg_set_line_width(pluto,l%stroke_width)
        call plutovg_stroke(pluto)

       
    end subroutine write_line
    

end module fig_bitmap_line
