module fig_bitmap_line
    use fig_shapes
    use fig_canvas
    use fig_bitmap_utils

contains

    subroutine write_line(canva, pixels, l)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(:,:), intent(inout):: pixels
        type(line), intent(in) :: l
        integer(pixel) :: color
        
        color = rgb_to_int(l%stroke_color)

        call draw_line(canva,pixels,l%x1,l%y1,l%x2,l%y2,color)
       
    end subroutine write_line
    

end module fig_bitmap_line
