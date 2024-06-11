program circle_test
    use fig_drawing
    use fig_shapes
    use fig_rgb
    use fig_rgb_color_constants
    use fig_svg
    use fig_bitmap
    implicit none

    integer, parameter :: CANVAS_WIDTH = 400
    integer, parameter :: CANVAS_HEIGHT = 400
    type(drawing) :: canva
    type(circle) :: circ
    type(RGB) :: bg, color
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva
    real :: cx, cy, radius

    call canva%init()
    bg = FIG_COLOR_GREEN
    bg%a = 150
    call canva%set_background(bg)

    ! Circle parameters
    cx = CANVAS_WIDTH / 2
    cy = CANVAS_HEIGHT / 3
    radius = min(CANVAS_WIDTH, CANVAS_HEIGHT) / 8

    circ%stroke_color = FIG_COLOR_BLACK

    circ%center%x = (cx + cx / 2) / CANVAS_WIDTH
    circ%center%y = cy / CANVAS_HEIGHT
    circ%r = radius
    circ%fill_color = FIG_COLOR_WHITE
    call canva%add_shape(circ)
    circ%r = radius / 2
    circ%fill_color = FIG_COLOR_BLUE
    call canva%add_shape(circ)
    circ%r = radius / 4
    circ%fill_color = FIG_COLOR_CYAN
    call canva%add_shape(circ)

    ! Left set of circles
    circ%center%x = (cx - cx / 2) / CANVAS_WIDTH
    circ%center%y = cy / CANVAS_HEIGHT
    circ%r = radius
    circ%fill_color = FIG_COLOR_WHITE
    call canva%add_shape(circ)
    circ%r = radius / 2
    circ%fill_color = FIG_COLOR_BLUE
    call canva%add_shape(circ)
    circ%r = radius / 4
    circ%fill_color = FIG_COLOR_CYAN
    call canva%add_shape(circ)

    ! Save to bitmap and SVG
    call bitmap_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT)
    call bitmap_canva%save_to_file(canva, 'green_blob')

    call svg_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT)
    call svg_canva%save_to_file(canva, 'green_blob')

    print *, 'Image successfully saved to green_blob.(ppm|svg)'

end program circle_test

