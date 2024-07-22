program test_fig_fill_triangle
    use fig_drawing
    use fig_shapes
    use fig_rgb
    use fig_rgb_color_constants
    use fig_svg
    use fig_bitmap
    use fig_test
    implicit none

    integer, parameter :: CANVAS_WIDTH = 801
    integer, parameter :: CANVAS_HEIGHT = 801
    integer, parameter :: TRIANGLE_SIZE = 50
    character(len=:), allocatable  :: file_name

    type(drawing) :: test_canvas
    type(triangle) :: tri
    type(circle) :: circ
    type(RGB), dimension(0:6) :: colors
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva

    integer :: i, j, ind, ind1, ind2

    file_name= "cool_triangle_pattern"
    ! Initialize the canvas
    call test_canvas%init()
    call test_canvas%set_background(FIG_COLOR_WHITE)

    colors = [ FIG_COLOR_RED,   & 
               FIG_COLOR_MAGENTA, &
               FIG_COLOR_YELLOW, &
               FIG_COLOR_GREEN,   &
               FIG_COLOR_CYAN,  &
               FIG_COLOR_PINK, &
               FIG_COLOR_TAN ]
     

    tri%stroke_color = FIG_COLOR_BLACK
    circ%stroke_color = FIG_COLOR_BLACK
    circ%r = 5.0

    ! Draw triangles and circles
    do i = 0, CANVAS_WIDTH - 1, TRIANGLE_SIZE
        do j = 0, CANVAS_HEIGHT - 1, TRIANGLE_SIZE
            ind = mod(i + 2 * j, 7)
            ind1 = mod(ind * 2, 7)
            ind2 = mod(ind * 3, 7)

            tri%p1%x = real(i) / CANVAS_WIDTH
            tri%p1%y = real(j) / CANVAS_HEIGHT
            tri%p2%x = real(i + TRIANGLE_SIZE) / CANVAS_WIDTH
            tri%p2%y = real(j) / CANVAS_HEIGHT
            tri%p3%x = real(i + TRIANGLE_SIZE) / CANVAS_WIDTH
            tri%p3%y = real(j + TRIANGLE_SIZE) / CANVAS_HEIGHT
            tri%fill_color = colors(ind1)
            call test_canvas%add_shape(tri)

            tri%p2%x = real(i) / CANVAS_WIDTH
            tri%p2%y = real(j + TRIANGLE_SIZE) / CANVAS_HEIGHT
            tri%fill_color = colors(ind2)
            call test_canvas%add_shape(tri)

            circ%center%x = real(i) / CANVAS_WIDTH
            circ%center%y = real(j) / CANVAS_HEIGHT
            circ%fill_color = colors(ind1)
            call test_canvas%add_shape(circ)

            circ%center%x = real(i + TRIANGLE_SIZE) / CANVAS_WIDTH
            call test_canvas%add_shape(circ)

            circ%center%y = real(j + TRIANGLE_SIZE) / CANVAS_HEIGHT
            call test_canvas%add_shape(circ)

            circ%center%x = real(i) / CANVAS_WIDTH
            call test_canvas%add_shape(circ)

            circ%center%x = real(i + TRIANGLE_SIZE / 2) / CANVAS_WIDTH
            circ%center%y = real(j + TRIANGLE_SIZE / 2) / CANVAS_HEIGHT
            call test_canvas%add_shape(circ)
        end do
    end do

    ! Save to bitmap and SVG
    call svg_canva%init(CANVAS_WIDTH,CANVAS_HEIGHT,file_name)
    call svg_canva%apply_shapes(test_canvas)
    call svg_canva%save_to_svg()
    call svg_canva%destroy()
    call bitmap_canva%init(CANVAS_WIDTH,CANVAS_HEIGHT,file_name)
    call bitmap_canva%apply_shapes(test_canvas)
    call bitmap_canva%save_to_png()
    call bitmap_canva%save_to_ppm()
    call bitmap_canva%destroy()


    call test_both(file_name,bitmap_canva)
contains 
    subroutine random_color(color)
        type(RGB) :: color
        real :: r, g, b
        call random_number(r)
        call random_number(g)
        call random_number(b)
        color%r = int(r * 255)
        color%g = int(g * 255)
        color%b = int(b * 255)
        color%a = 255
    end subroutine random_color
end program test_fig_fill_triangle

