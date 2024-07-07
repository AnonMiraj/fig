
program test_fig_draw_triangle
    use fig_drawing
    use fig_shapes
    use fig_rgb_color_constants
    use fig_rgb
    use fig_svg
    use fig_bitmap
    use fig_test
    implicit none

    integer, parameter :: CANVAS_WIDTH = 801
    integer, parameter :: CANVAS_HEIGHT = 801
    integer, parameter :: TRIANGLE_SIZE = 50
    character(len=:), allocatable  :: file_name

    type(drawing) :: test_canvas
    type(triangle) :: tri
    type(RGB), dimension(0:6) :: colors
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva

    integer :: i, j, ind

    file_name= "triangles"
    ! Initialize the canvas
    call test_canvas%init()
    call test_canvas%set_background(FIG_COLOR_WHITE)

    ! Define colors in the array
    colors(0) = FIG_COLOR_RED
    colors(1) = FIG_COLOR_PINK
    colors(2) = FIG_COLOR_YELLOW
    colors(3) = FIG_COLOR_GREEN
    colors(4) = FIG_COLOR_BLUE
    colors(5) = FIG_COLOR_CYAN
    colors(6) = FIG_COLOR_MAGENTA

    tri%fill_color = FIG_COLOR_BLACK

    ! Draw triangles
    do i = 0, CANVAS_WIDTH - 1, TRIANGLE_SIZE
        do j = 0, CANVAS_HEIGHT - 1, TRIANGLE_SIZE
            ind = mod(i + j, 7)
            tri%p1%x = real(i) / CANVAS_WIDTH
            tri%p1%y = real(j) / CANVAS_HEIGHT
            tri%p2%x = real(i + TRIANGLE_SIZE) / CANVAS_WIDTH
            tri%p2%y = real(j) / CANVAS_HEIGHT
            tri%p3%x = real(i + TRIANGLE_SIZE) / CANVAS_WIDTH
            tri%p3%y = real(j + TRIANGLE_SIZE) / CANVAS_HEIGHT
            tri%stroke_color = colors(ind)

            call test_canvas%add_shape(tri)

            tri%p2%x = real(i) / CANVAS_WIDTH
            tri%p2%y = real(j + TRIANGLE_SIZE) / CANVAS_HEIGHT
            call test_canvas%add_shape(tri)
        end do
    end do

    ! Save to bitmap and SVG
    call svg_canva%init(CANVAS_WIDTH,CANVAS_HEIGHT,file_name)
    call svg_canva%apply_shapes(test_canvas)
    call svg_canva%save_to_svg()
    call svg_canva%destroy()
    call bitmap_canva%init(CANVAS_WIDTH,CANVAS_HEIGHT,file_name)
    call bitmap_canva%apply_shapes(test_canvas)
    call bitmap_canva%save_to_png()
    call bitmap_canva%save_to_ppm()
    call bitmap_canva%destroy()


    call test_both(file_name,bitmap_canva)
end program test_fig_draw_triangle

