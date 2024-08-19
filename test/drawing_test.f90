
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
    type(line) :: l
    type(ellipse) :: elp
    type(arc) :: ar
    type(text) :: te
    type(RGB) :: bg, color
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva
    real(8) :: pi = 4.0d0 * atan(1.0d0)
    file_name = "test_all"
    bg = FIG_COLOR_WHITE

    call canva%init()
    canva%background=FIG_COLOR_WHITE

   ! Circle
    c%c%x = 100.0 / CANVAS_WIDTH
    c%c%y = 100.0 / CANVAS_HEIGHT
    c%r = 50.0
    c%fill_color = FIG_COLOR_PINK
    c%fill_color%a = .5
    c%stroke_color = FIG_COLOR_RED
    c%stroke_color%a = .5
    call canva%add_shape(c)

    ! Ellipse
    elp%c%x = 250.0 / CANVAS_WIDTH
    elp%c%y = 100.0 / CANVAS_HEIGHT
    elp%rx = 50.0 
    elp%ry = 25.0 
    color = FIG_COLOR_GOLD
    color%a = .5
    elp%fill_color = color
    elp%stroke_color = FIG_COLOR_RED
    elp%stroke_color%a = .5
    call canva%add_shape(elp)

    ! Rectangle 1
    r%p%x = 200.0 / CANVAS_WIDTH
    r%p%y = 200.0 / CANVAS_HEIGHT
    r%width = 100.0 
    r%height = 50.0 
    r%fill_color = FIG_COLOR_BLUE
    r%fill_color%a = .5
    r%stroke_width = 10
    r%stroke_color = FIG_COLOR_GOLD
    call canva%add_shape(r)

    ! Rectangle 2
    r%p%x = 250.0 / CANVAS_WIDTH
    r%p%y = 220.0 / CANVAS_HEIGHT
    r%width = 120.0 
    r%height = 50.0 
    r%fill_color = FIG_COLOR_RED
    r%fill_color%a = .5
    r%rx=5
    r%ry=5
    r%stroke_width = 6
    r%stroke_color = FIG_COLOR_SEAGREEN
    r%stroke_color%a=.5
    call canva%add_shape(r)

    ! Line 1
    l%p1%x = 400.0 / CANVAS_WIDTH
    l%p1%y = 200.0 / CANVAS_HEIGHT
    l%p2%x = 550.0 / CANVAS_WIDTH
    l%p2%y = 450.0 / CANVAS_HEIGHT
    l%stroke_color = FIG_COLOR_BLACK
    l%stroke_width = 10
    call canva%add_shape(l)
    l%stroke_width = 2
    l%stroke_color = FIG_COLOR_RED
    l%stroke_color%a=.5
    call canva%add_shape(l)


    ! Line
    l%p1%x = 400.0 / CANVAS_WIDTH
    l%p1%y = 400.0 / CANVAS_HEIGHT
    l%p2%x = 550.0 / CANVAS_WIDTH
    l%p2%y = 400.0 / CANVAS_HEIGHT
    l%stroke_color = FIG_COLOR_BLACK
    l%stroke_color%a=.5
    l%stroke_width = 50
    call canva%add_shape(l)
    l%stroke_width = 2
    l%stroke_color = FIG_COLOR_RED
    l%stroke_color%a=.5

    call canva%add_shape(l)


    ! Arc
    ar%c%x = 300.0 / CANVAS_WIDTH
    ar%c%y = 400.0 / CANVAS_HEIGHT
    ar%r = 50.0
    ar%start_angle= 0
    ar%end_angle= pi * 1.2
    ar%fill_color = FIG_COLOR_INDIGO
    ar%fill_color%a=.5
    ar%stroke_color = FIG_COLOR_BROWN
    call canva%add_shape(ar)
   
    
    ! Text
    te%p%x=.0
    te%p%y=.5
    te%slant=FIG_FONT_SLANT_ITALIC
    te%weight=FIG_FONT_WEIGHT_BOLD
    te%size = 20
    te%content="Hello, Fortran"
    te%font_family="sans"
    te%fill_color=FIG_COLOR_RED
    te%stroke_color=FIG_COLOR_BLACK
    call canva%add_shape(te)

    ! Text
    te%p%x=.0
    te%p%y=.7
    te%slant=FIG_FONT_SLANT_OBLIQUE
    te%weight=FIG_FONT_WEIGHT_NORMAL
    te%size = 20
    te%content="Hello, Fortarn"
    te%font_family="sans"
    te%fill_color=FIG_COLOR_RED
    te%stroke_color=FIG_COLOR_BLACK
    ! call canva%add_shape(te)


    call svg_canva%init(CANVAS_WIDTH,CANVAS_HEIGHT,file_name)
    call svg_canva%apply_shapes(canva)
    call svg_canva%save_to_svg()
    call svg_canva%destroy()

    call bitmap_canva%init(CANVAS_WIDTH,CANVAS_HEIGHT,file_name)
    call bitmap_canva%apply_shapes(canva)
    call bitmap_canva%save_to_png()
    call bitmap_canva%save_to_ppm()
    call bitmap_canva%destroy()

    call test_both(file_name)
end program drawing_test_all

