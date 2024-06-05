module fig_bitmap_triangle
    use fig_shapes
    use fig_canvas
    use fig_bitmap_utils

contains

    subroutine write_triangle(canva, pixels, tri)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(:,:), intent(inout):: pixels
        type(triangle), intent(in) :: tri
        call draw_inner_triangle(canva,pixels,tri)
        call draw_outer_triangle(canva,pixels,tri)
    end subroutine write_triangle

    subroutine draw_outer_triangle(canva, pixels, tri)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(:,:), intent(inout):: pixels
        type(triangle), intent(in) :: tri
        integer(pixel) :: color
        color = rgb_to_int(tri%stroke_color)

        call draw_line(canva,pixels,tri%x1,tri%y1,tri%x2,tri%y2,color)
        call draw_line(canva,pixels,tri%x2,tri%y2,tri%x3,tri%y3,color)
        call draw_line(canva,pixels,tri%x3,tri%y3,tri%x1,tri%y1,color)
        
    end subroutine draw_outer_triangle

    subroutine draw_inner_triangle(canva, pixels, tri)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(:,:), intent(inout):: pixels
        type(triangle), intent(in) :: tri
        integer(pixel) :: color
        color = rgb_to_int(tri%fill_color)

        call fill_triangle(canva,pixels,int(tri%x1),int(tri%y1),int(tri%x2),int(tri%y2),int(tri%x3),int(tri%y3),color)
        
    end subroutine draw_inner_triangle


    

end module fig_bitmap_triangle
