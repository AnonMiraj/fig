program path_test
    use fig_path
    use fig_drawing
    use fig_svg
    use fig_bitmap
    use fig_test
    use fig_rgb_color_constants
    implicit none
    integer, parameter :: CANVAS_WIDTH = 400
    integer, parameter :: CANVAS_HEIGHT = 400
    character(len=:), allocatable  :: file_name
    type(drawing) :: canva
    type(path) :: my_path
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva

    call canva%init()
    file_name= "path_example"
    call my_path%moveTo(10.0/CANVAS_WIDTH, 10.0/CANVAS_HEIGHT)
    call my_path%lineTo(50.0/CANVAS_WIDTH, 20.0/CANVAS_HEIGHT)
    call my_path%bezierCurveTo(30.0/CANVAS_WIDTH, 40.0/CANVAS_HEIGHT,&
        60.0/CANVAS_WIDTH, 50.0/CANVAS_HEIGHT, 80.0/CANVAS_WIDTH, 30.0/CANVAS_HEIGHT)
    call my_path%quadraticCurveTo(100.0/CANVAS_WIDTH, 50.0/CANVAS_HEIGHT,&
        120.0/CANVAS_WIDTH, 80.0/CANVAS_HEIGHT)
    call my_path%ellipticalArcTo(40.0/CANVAS_WIDTH, 20.0/CANVAS_HEIGHT,&
        30.0, 1, 0, 150.0/CANVAS_WIDTH, 100.0/CANVAS_HEIGHT)
    call my_path%closePath()
    my_path%fill_color = FIG_COLOR_LIGHTGOLDENRODYELLOW
    my_path%stroke_color = FIG_COLOR_AQUA
    call canva%add_shape(my_path)
    print *,my_path%path_string
    
    ! Save to bitmap and SVG
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

