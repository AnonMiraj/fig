program test_fig_draw_triangle
    use fig_drawing
    use fig_shapes
    use fig_rgb_color_constants
    use fig_rgb
    implicit none

    integer :: result
    type(drawing) :: test_canvas
    type(triangle):: tri
    type(RGB), dimension(0:6) :: colors
    integer :: i, j, ind=1
    integer :: triangle_size = 50

    ! Initialize canvas and colors
    call test_canvas%init(801.0,801.0,"Triangles")
    

    ! Define colors in the array
    colors(0) = FIG_COLOR_RED
    colors(1) = FIG_COLOR_PINK
    colors(2) = FIG_COLOR_YELLOW
    colors(3) = FIG_COLOR_GREEN
    colors(4) = FIG_COLOR_BLUE
    colors(5) = FIG_COLOR_CYAN
    colors(6) = FIG_COLOR_MAGENTA

    tri%fill_color=FIG_COLOR_BLACK

    do i = 0, 799, triangle_size
        do j = 0, 799, triangle_size
            ind =mod(i+j,7)
            tri%x1=i
            tri%y1=j
            tri%x2=i + triangle_size
            tri%y2=j
            tri%x3=i + triangle_size
            tri%y3=j + triangle_size
            tri%stroke_color=colors(ind)

            call test_canvas%add_shape(tri)

            tri%x2=i 
            tri%y2=j + triangle_size
            call test_canvas%add_shape(tri)

        end do
    end do

    call test_canvas%save_to_file("ppm")
    call test_canvas%save_to_file("svg")
    print *, "drawing exported successfully: Triangles.(ppm\svg)"
end program test_fig_draw_triangle
