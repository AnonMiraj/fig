program drawing_test_all
    use fig_drawing
    use fig_shapes
    use fig_rgb
    implicit none

    type(drawing) :: canva
    type(circle) :: c
    type(rectangle) :: r
    type(RGB):: bg
    bg=GREEN
    bg%a=50
    call canva%init(800.0,800.0,'output')
    call canva%set_background(bg)

    c%cx = 100.0
    c%cy = 100.0
    c%r = 50.0
    c%fill_color = PINK
    call canva%add_shape(c)

    r%x = 200.0
    r%y = 200.0
    r%width = 100.0
    r%height = 50.0
    r%fill_color = BLUE
    call canva%add_shape(r)

    r%x = 250.0
    r%y = 260.0
    r%width = 120.0
    r%height = 54.0
    r%fill_color = RED
    call canva%add_shape(r)

    call canva%save_to_file('svg')
    call canva%save_to_file('ppm')

    print *, "drawing exported successfully: output.(ppm/svg)"
end program drawing_test_all
