program drawing_test_all
    use fig_drawing
    use fig_shapes
    use fig_rgb
    use fig_rgb_color_constants
    implicit none

    type(drawing) :: canva
    type(circle) :: c
    type(rectangle) :: r
    type(triangle) :: tri
    type(line) :: l
    type(ellipse) :: elp
    type(RGB) :: bg, color
    bg = FIG_COLOR_WHITE

    call canva%init(600.0, 600.0, 'output')
    call canva%set_background(bg)

    ! Circle
    c%cx = 100.0
    c%cy = 100.0
    c%r = 50.0
    c%fill_color = FIG_COLOR_PINK
    c%fill_color%a = 100
    c%stroke_color = FIG_COLOR_RED
    c%stroke_color%a = 100
    call canva%add_shape(c)

    ! Ellipse
    elp%cx = 250.0
    elp%cy = 100.0
    elp%rx = 50.0
    elp%ry = 25.0
    color = FIG_COLOR_GOLD
    color%a = 100
    elp%fill_color = color
    elp%stroke_color = FIG_COLOR_RED
    elp%stroke_color%a = 100
    call canva%add_shape(elp)

    ! Rectangle 1
    r%x = 200.0
    r%y = 200.0
    r%width = 100.0
    r%height = 50.0
    r%fill_color = FIG_COLOR_BLUE
    r%fill_color%a = 100
    call canva%add_shape(r)

    ! Rectangle 2
    r%x = 250.0
    r%y = 220.0
    r%width = 120.0
    r%height = 50.0
    r%fill_color = FIG_COLOR_RED
    r%fill_color%a = 100
    call canva%add_shape(r)

    ! Line
    l%x1 = 400.0
    l%y1 = 200.0
    l%x2 = 550.0
    l%y2 = 450.0
    l%stroke_color = FIG_COLOR_BLACK
    call canva%add_shape(l)


    ! Triangle
    tri%x1 = 450.0
    tri%y1 = 150.0
    tri%x2 = 550.0
    tri%y2 = 250.0
    tri%x3 = 450.0
    tri%y3 = 350.0
    tri%fill_color = FIG_COLOR_SILVER
    tri%fill_color%a = 100
    tri%stroke_color = FIG_COLOR_RED
    call canva%add_shape(tri)
    call canva%save_to_file('svg')
    call canva%save_to_file('ppm')

    print *, "Drawing exported successfully: output.(ppm|svg)"
end program drawing_test_all
