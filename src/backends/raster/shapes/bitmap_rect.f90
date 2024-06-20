module fig_bitmap_rect
    use fig_shapes
    use fig_canvas
    use fig_bitmap_utils
    use fig_bitmap_line_utils

contains

    subroutine write_rectangle(canva, pixels, rect)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(:,:), intent(inout):: pixels
        type(rectangle), intent(in) :: rect
        
        call draw_inner_rect(canva, pixels, rect)
        call draw_outer_rect(canva, pixels, rect)
    end subroutine write_rectangle
    
    subroutine draw_inner_rect(canva, pixels, rect)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(:,:), intent(inout):: pixels
        type(rectangle), intent(in) :: rect
        integer(pixel) :: color
        type(canvas_point) :: p
        p= to_canvas(rect%upper_left,canva%size)
        color = rgb_to_int(rect%fill_color)
        call fill_rect(canva, pixels, p%x, p%y, int(rect%width), int(rect%height), color)

    end subroutine draw_inner_rect

    subroutine draw_outer_rect(canva, pixels, rect)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(:,:), intent(inout):: pixels
        type(rectangle), intent(in) :: rect
        integer(pixel) :: color
        type(canvas_point) :: p
        p= to_canvas(rect%upper_left,canva%size)
        color = rgb_to_int(rect%stroke_color)

        call draw_line(canva,pixels,&
            p%x, p%y, p%x + int(rect%width), p%y, color) ! Top line
        call draw_line(canva,pixels,&
            p%x, p%y, p%x, p%y + int(rect%height), color) ! Left line
        call draw_line(canva,pixels,&
            p%x + int(rect%width) - 1, p%y + int(rect%height) - 1, p%x + int(rect%width) - 1, p%y, color) ! Right line
        call draw_line(canva,pixels,&
            p%x + int(rect%width) - 1, p%y + int(rect%height) - 1, p%x, p%y + int(rect%height) - 1, color) ! Bottom line
    end subroutine draw_outer_rect



end module fig_bitmap_rect
