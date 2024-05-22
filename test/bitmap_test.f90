
program test_bitmap_writer
    use fig_bitmap
    use fig_shapes
    use fig_rgb
    implicit none

    type(bitmap_canvas) :: canva
    type(circle) :: c
    type(rectangle) :: r

    call canva%init(800.0,800.0,'output')

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

    call canva%save_to_file()

    print *, "bitmap file created successfully: output.ppm"
end program test_bitmap_writer
