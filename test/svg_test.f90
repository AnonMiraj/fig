
program test_svg_writer
    use fig_svg
    use fig_shapes
    implicit none

    type(svg_canvas) :: svg
    type(circle) :: c
    type(rectangle) :: r

    call svg%init('output.svg')
    svg%height=800
    svg%width=800

    c%cx = 100.0
    c%cy = 100.0
    c%r = 50.0
    call svg%add_shape(c)

    r%x = 200.0
    r%y = 200.0
    r%width = 100.0
    r%height = 50.0
    r%fill_color = 'blue'

    call svg%add_shape(r)
    r%x = 250.0
    r%y = 260.0
    r%width = 120.0
    r%height = 54.0
    r%fill_color = 'red'
    call svg%add_shape(r)

    call svg%save_to_file()

    print *, "SVG file created successfully: output.svg"
end program test_svg_writer
