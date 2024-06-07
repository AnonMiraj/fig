program circle_test
    use fig_drawing
    use fig_shapes
    use fig_rgb
    use fig_rgb_color_constants
    implicit none

    type(drawing)  :: c
    type(rectangle) :: rect
    type(circle) :: circ
    integer :: cx, cy, radius, result
    type(RGB) :: color
    call c%init(400.0, 400.0, "green_blob")

    color = FIG_COLOR_GREEN
    color%a =150
    cx = int(c%width) / 2 
    cy = int(c%height) / 3
    radius = min(int(c%width), int(c%height)) / 8 

    

    call c%set_background(color)

    circ%stroke_color= FIG_COLOR_BLACK

    circ%cx= cx+cx/2
    circ%cy= cy
    circ%r= radius
    circ%fill_color= FIG_COLOR_WHITE
    call c%add_shape(circ)
    circ%r= radius/2
    circ%fill_color= FIG_COLOR_BLUE
    call c%add_shape(circ)
    circ%r= radius/4
    circ%fill_color= FIG_COLOR_CYAN
    call c%add_shape(circ)

    circ%cx= cx-cx/2
    circ%cy= cy
    circ%r= radius
    circ%fill_color= FIG_COLOR_WHITE
    call c%add_shape(circ)
    circ%r= radius/2
    circ%fill_color= FIG_COLOR_BLUE
    call c%add_shape(circ)
    circ%r= radius/4
    circ%fill_color= FIG_COLOR_CYAN
    call c%add_shape(circ)




    call c%save_to_file('svg')
    call c%save_to_file('ppm')

    print *, 'Image successfully saved to green_blob.(ppm\svg)'

end program circle_test
