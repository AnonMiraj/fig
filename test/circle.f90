program circle_test
    !!use fig_g_canvas
    use fig_shapes
    use fig_rgb
    implicit none

    !!type(g_canvas)  :: c
    type(rectangle) :: rect
    type(circle) :: circ
    integer :: cx, cy, radius, result
    !!call c%init(400.0, 400.0, "green_blob")

    !!cx = int(c%width) / 2 
    !!cy = int(c%height) / 3
    !!radius = min(int(c%width), int(c%height)) / 8 


    rect%width=400
    rect%height=400
    rect%fill_color=GREEN

    !!call c%add_shape(rect)
    circ%stroke_color= BLACK

    circ%cx= cx+cx/2
    circ%cy= cy
    circ%r= radius
    circ%fill_color= WHITE
    !!call c%add_shape(circ)
    circ%r= radius/2
    circ%fill_color= BLUE
    !!call c%add_shape(circ)
    circ%r= radius/4
    circ%fill_color= CYAN
    !!call c%add_shape(circ)

    circ%cx= cx-cx/2
    circ%cy= cy
    circ%r= radius
    circ%fill_color= WHITE
    !!call c%add_shape(circ)
    circ%r= radius/2
    circ%fill_color= BLUE
    !!call c%add_shape(circ)
    circ%r= radius/4
    circ%fill_color= CYAN
    !!call c%add_shape(circ)




    !!call c%save_to_file('svg')
    !!call c%save_to_file('ppm')

    print *, 'Image successfully saved to green_blob.(ppm\svg)'

end program circle_test
