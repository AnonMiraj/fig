program path_test
    use fig_poly
    use fig_drawing
    use fig_svg
    use fig_types
    use fig_bitmap
    use fig_test
    use fig_rgb_color_constants
    use fig_cairo_poly

    implicit none
    integer, parameter :: CANVAS_WIDTH = 500
    integer, parameter :: CANVAS_HEIGHT = 180
    character(len=:), allocatable  :: file_name
    type(drawing) :: canva
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva
    type(polyline) :: pl
    real :: x_points(7) = [0.0, 40.0, 40.0, 80.0, 80.0, 120.0, 120.0]
    real :: y_points(7) = [40.0, 40.0, 80.0, 80.0, 120.0, 120.0, 160.0]
    integer :: n=7,i
    type(point), allocatable:: points(:)
    integer :: p_count, real_ind

    x_points=x_points/CANVAS_WIDTH
    y_points=y_points/CANVAS_HEIGHT
    call canva%init()
    file_name= "poly_example"
    call pl%add_points(x_points, y_points, n)
    pl%stroke_color=FIG_COLOR_RED
    pl%fill_color=FIG_COLOR_YELLOW
    pl%stroke_width=4
    call canva%add_shape(pl)

    call bitmap_canva%init(CANVAS_WIDTH,CANVAS_HEIGHT,file_name)
    call bitmap_canva%apply_shapes(canva)
    call bitmap_canva%save_to_png()
    call bitmap_canva%save_to_ppm()
    call bitmap_canva%destroy()

    call svg_canva%init(CANVAS_WIDTH,CANVAS_HEIGHT,file_name)
    call svg_canva%apply_shapes(canva)
    call svg_canva%save_to_svg()
    call svg_canva%destroy()
    call test_both(file_name,bitmap_canva)

end program path_test

