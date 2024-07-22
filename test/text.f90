program smile
    use fig_drawing
    use fig_shapes
    use fig_rgb_color_constants
    use fig_rgb
    use fig_svg
    use fig_bitmap
    use fig_test
    implicit none

    integer, parameter :: CANVAS_WIDTH = 400
    integer, parameter :: CANVAS_HEIGHT = 400
    integer, parameter :: FACE_RADIUS = 100
    integer, parameter :: EYE_RADIUS = 25
    integer, parameter :: MOUTH_RADIUS_X = 50
    integer, parameter :: MOUTH_RADIUS_Y = 25
    character(len=:), allocatable  :: file_name

    type(drawing) :: canva
    type(text) :: te
    type(rectangle) :: rec
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva

    file_name= "text_example"
    ! Initialize the canvas
    call canva%init()
    call canva%set_background(FIG_COLOR_WHITE)


    
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

    te%p%x=.0
    te%p%y=.7
    te%slant=FIG_FONT_SLANT_OBLIQUE
    te%weight=FIG_FONT_WEIGHT_NORMAL
    te%size = 20
    te%content="Hello, Fortarn"
    te%font_family="sans"
    te%fill_color=FIG_COLOR_RED
    te%stroke_color=FIG_COLOR_BLACK
    call canva%add_shape(te)

    ! Save to bitmap and SVG
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

end program smile

