
program drawing_test_all
    use fig_drawing
    use fig_shapes
    use fig_rgb
    use fig_rgb_color_constants
    use fig_svg
    use fig_bitmap
    use fig_test
    implicit none

    integer, parameter :: CANVAS_WIDTH = 600.0
    integer, parameter :: CANVAS_HEIGHT = 600.0
    character(len=:), allocatable  :: file_name
    type(drawing) :: canva
    type(circle) :: c
    type(rectangle) :: r
    type(triangle) :: tri
    type(line) :: l
    type(ellipse) :: elp
    type(arc) :: ar
    type(RGB) :: bg, color
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva
    real(8) :: pi = 4.0d0 * atan(1.0d0)
    file_name = "test_all"
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
    r%stroke_width = 10
    r%stroke_color = FIG_COLOR_GOLD
    call canva%add_shape(r)

    ! Rectangle 2
    r%upper_left%x = 250.0 / CANVAS_WIDTH
    r%upper_left%y = 220.0 / CANVAS_HEIGHT
    r%width = 120.0 
    r%height = 50.0 
    r%fill_color = FIG_COLOR_RED
    r%fill_color%a = 100
    r%rx=5
    r%ry=5
    r%stroke_width = 6
    r%stroke_color = FIG_COLOR_SEAGREEN
    r%stroke_color%a=150
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


     ! arc
    ar%center%x = 300.0 / CANVAS_WIDTH
    ar%center%y = 400.0 / CANVAS_HEIGHT
    ar%r = 50.0
    ar%start_angle= 0
    ar%end_angle= pi * 1.2
    ar%fill_color = FIG_COLOR_INDIGO
    ar%fill_color%a=100
    ar%stroke_color = FIG_COLOR_BROWN
    call canva%add_shape(ar)
   
    call svg_canva%init(CANVAS_WIDTH,CANVAS_HEIGHT,file_name)
    call svg_canva%apply_shapes(canva)
    call svg_canva%save_to_svg()
    call svg_canva%destroy()

    call bitmap_canva%init(CANVAS_WIDTH,CANVAS_HEIGHT,file_name)
    call bitmap_canva%apply_shapes(canva)
    call bitmap_canva%save_to_png()
    call bitmap_canva%save_to_ppm()
    call bitmap_canva%destroy()

    call test_both(file_name,bitmap_canva)
end program drawing_test_all

