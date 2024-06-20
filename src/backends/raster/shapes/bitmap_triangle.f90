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
        type(canvas_point) :: p1,p2,p3
        p1= to_canvas(tri%p1,canva%size)
        p2= to_canvas(tri%p2,canva%size)
        p3= to_canvas(tri%p3,canva%size)
        color = rgb_to_int(tri%stroke_color)

        call draw_line(canva,pixels,p1%x,p1%y,p2%x,p2%y,color)
        call draw_line(canva,pixels,p2%x,p2%y,p3%x,p3%y,color)
        call draw_line(canva,pixels,p3%x,p3%y,p1%x,p1%y,color)
        
    end subroutine draw_outer_triangle

    subroutine draw_inner_triangle(canva, pixels, tri)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(:,:), intent(inout):: pixels
        type(triangle), intent(in) :: tri
        integer(pixel) :: color
        type(canvas_point) :: p1,p2,p3
        p1= to_canvas(tri%p1,canva%size)
        p2= to_canvas(tri%p2,canva%size)
        p3= to_canvas(tri%p3,canva%size)
        color = rgb_to_int(tri%fill_color)

        call fill_triangle(canva,pixels,int(p1%x),int(p1%y),int(p2%x),int(p2%y),int(p3%x),int(p3%y),color)
        
    end subroutine draw_inner_triangle


    

end module fig_bitmap_triangle
