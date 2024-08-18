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
    integer, parameter :: CANVAS_HEIGHT = 210
    character(len=:), allocatable  :: file_name
    type(drawing) :: canva
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva
    type(polyline) :: pl
    type(polygon) :: pg
    real :: x_points(7) = [0.0, 40.0, 40.0, 80.0, 80.0, 120.0, 120.0]
    real :: y_points(7) = [40.0, 40.0, 80.0, 80.0, 120.0, 120.0, 160.0]

    real :: polygon_x_points(5) = [200.0, 140.0, 290.0, 110.0, 260.0]
    real :: polygon_y_points(5) = [10.0, 198.0, 78.0, 78.0, 198.0]
    integer :: n=7,n2=5

    x_points=x_points/CANVAS_WIDTH
    y_points=y_points/CANVAS_HEIGHT

    polygon_x_points=polygon_x_points/CANVAS_WIDTH
    polygon_y_points=polygon_y_points/CANVAS_HEIGHT
    call canva%init()
    file_name= "poly_example"
    call pl%add_points(x_points, y_points, n)
    pl%stroke_color=FIG_COLOR_RED
    pl%fill_color=FIG_COLOR_YELLOW
    pl%stroke_width=4
    call canva%add_shape(pl)

    call pg%add_points(polygon_x_points, polygon_y_points, n2)
    pg%stroke_color=FIG_COLOR_BLACK
    pg%fill_color=FIG_COLOR_YELLOW
    pg%stroke_width=4
    call canva%add_shape(pg)


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

