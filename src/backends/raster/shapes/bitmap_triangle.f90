module fig_bitmap_triangle
    use plutovg
    use fig_shapes
    use fig_canvas
    use fig_bitmap_utils

contains

    subroutine write_triangle(canva, pluto, tri)
        class(base_canvas), intent(inout) :: canva
        type(c_ptr), intent(inout):: pluto
        type(triangle), intent(in) :: tri
        type(canvas_point) :: p1,p2,p3

        p1 = to_canvas ( tri%p1 , canva%size)
        p2 = to_canvas ( tri%p2 , canva%size)
        p3 = to_canvas ( tri%p3 , canva%size)

        call plutovg_new_path(pluto)
        call plutovg_move_to(pluto,p1%x,p1%y)
        call plutovg_line_to(pluto,p2%x,p2%y)
        call plutovg_line_to(pluto,p3%x,p3%y)
        call plutovg_line_to(pluto,p1%x,p1%y)
        call plutovg_close_path(pluto)
        call fill(pluto,tri%fill_color)
        call stroke(pluto,tri%stroke_color,tri%stroke_width)

    end subroutine write_triangle
   

end module fig_bitmap_triangle
