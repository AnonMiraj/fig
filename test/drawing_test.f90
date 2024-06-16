
program drawing_test_all
    use fig_drawing
    use fig_shapes
    use fig_rgb
    use fig_rgb_color_constants
    use fig_svg
    use fig_bitmap
    implicit none

    integer, parameter :: CANVAS_WIDTH = 600.0
    integer, parameter :: CANVAS_HEIGHT = 600.0
    type(drawing) :: canva
    type(circle) :: c
    type(rectangle) :: r
    type(triangle) :: tri
    type(line) :: l
    type(ellipse) :: elp
    type(RGB) :: bg, color
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva
    bg = FIG_COLOR_WHITE

    call canva%init()
    call canva%set_background(bg)

    ! Circle
    c%center%x = 100.0 / CANVAS_WIDTH
    c%center%y = 100.0 / CANVAS_HEIGHT
    c%r = 50.0
    c%fill_color = FIG_COLOR_PINK
    c%fill_color%a = 100
    c%stroke_color = FIG_COLOR_RED
    c%stroke_color%a = 100
    call canva%add_shape(c)

    ! Ellipse
    elp%center%x = 250.0 / CANVAS_WIDTH
    elp%center%y = 100.0 / CANVAS_HEIGHT
    elp%rx = 50.0 
    elp%ry = 25.0 
    color = FIG_COLOR_GOLD
    color%a = 100
    elp%fill_color = color
    elp%stroke_color = FIG_COLOR_RED
    elp%stroke_color%a = 100
    call canva%add_shape(elp)

    ! Rectangle 1
    r%upper_left%x = 200.0 / CANVAS_WIDTH
    r%upper_left%y = 200.0 / CANVAS_HEIGHT
    r%width = 100.0 
    r%height = 50.0 
    r%fill_color = FIG_COLOR_BLUE
    r%fill_color%a = 100
    call canva%add_shape(r)

    ! Rectangle 2
    r%upper_left%x = 250.0 / CANVAS_WIDTH
    r%upper_left%y = 220.0 / CANVAS_HEIGHT
    r%width = 120.0 
    r%height = 50.0 
    r%fill_color = FIG_COLOR_RED
    r%fill_color%a = 100
    call canva%add_shape(r)

    ! Line
    l%p1%x = 400.0 / CANVAS_WIDTH
    l%p1%y = 200.0 / CANVAS_HEIGHT
    l%p2%x = 550.0 / CANVAS_WIDTH
    l%p2%y = 450.0 / CANVAS_HEIGHT
    l%stroke_color = FIG_COLOR_BLACK
    l%stroke_width = 10
    call canva%add_shape(l)
    l%stroke_width = 2
    l%stroke_color = FIG_COLOR_RED
    l%stroke_color%a=100

    call canva%add_shape(l)


    ! Line
    l%p1%x = 400.0 / CANVAS_WIDTH
    l%p1%y = 400.0 / CANVAS_HEIGHT
    l%p2%x = 550.0 / CANVAS_WIDTH
    l%p2%y = 400.0 / CANVAS_HEIGHT
    l%stroke_color = FIG_COLOR_BLACK
    l%stroke_color%a=100
    l%stroke_width = 50
    call canva%add_shape(l)
    l%stroke_width = 2
    l%stroke_color = FIG_COLOR_RED
    l%stroke_color%a=100

    call canva%add_shape(l)


    ! Triangle
    tri%p1%x = 450.0 / CANVAS_WIDTH
    tri%p1%y = 150.0 / CANVAS_HEIGHT
    tri%p2%x = 550.0 / CANVAS_WIDTH
    tri%p2%y = 250.0 / CANVAS_HEIGHT
    tri%p3%x = 450.0 / CANVAS_WIDTH
    tri%p3%y = 350.0 / CANVAS_HEIGHT
    tri%fill_color = FIG_COLOR_SILVER
    tri%fill_color%a = 100
    tri%stroke_color = FIG_COLOR_RED
    call canva%add_shape(tri)
    
    call bitmap_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT)
    call bitmap_canva%save_to_file(canva,'test_all')

    call svg_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT)
    call svg_canva%save_to_file(canva,'test_all')

    print *, "Drawing exported successfully: test_all.(ppm|svg)"
end program drawing_test_all

