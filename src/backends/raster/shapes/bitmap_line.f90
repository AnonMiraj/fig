module fig_bitmap_line
    use fig_shapes
    use fig_canvas
    use fig_bitmap_utils
    use fig_bitmap_line_utils

contains

    subroutine write_line(canva, pixels, l)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(:,:), intent(inout):: pixels
        type(line), intent(in) :: l
        integer(pixel) :: color
        type(canvas_point) :: p1,p2
        p1= to_canvas(l%p1,canva%size)
        p2= to_canvas(l%p2,canva%size)
        color = rgb_to_int(l%stroke_color)


        call draw_thick_line(canva,pixels,p1%x,p1%y,p2%x,p2%y,l%stroke_width,color)
       
    end subroutine write_line
    

end module fig_bitmap_line
