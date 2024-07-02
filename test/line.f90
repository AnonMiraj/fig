program radial_lines
    use fig_drawing
    use fig_shapes
    use fig_rgb_color_constants
    use fig_rgb
    use fig_svg
    use fig_bitmap
    use fig_test
    implicit none

    integer, parameter :: CANVAS_WIDTH = 800
    integer, parameter :: CANVAS_HEIGHT = 800
    integer, parameter :: NUM_LINES = 180
    character(len=:), allocatable  :: file_name

    type(drawing) :: radial_canvas
    type(line) :: sh
    type(RGB) :: color
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva
    type(RGB), dimension(8) :: color_palette
    integer :: i,ind=0
    real :: cx, cy, radius
    real :: angle_step, angle
    file_name= "radial_lines"
 
    color_palette = [ FIG_COLOR_RED,   & 
                      FIG_COLOR_MAGENTA, &
                      FIG_COLOR_YELLOW, &
                      FIG_COLOR_GREEN,   &
                      FIG_COLOR_BLUE,   &
                      FIG_COLOR_CYAN,  &
                      FIG_COLOR_PINK, &
                      FIG_COLOR_WHITE ]
     
    call radial_canvas%init()
    call radial_canvas%set_background(FIG_COLOR_BLACK)

    cx = CANVAS_WIDTH / 2
    cy = CANVAS_HEIGHT / 2
    radius = 300
    angle_step = 1.0 * atan(1.0) / NUM_LINES

    do i = 0, NUM_LINES - 1
        call random_color(color)
        angle = 4 * i * angle_step
        call draw_radial_line(radial_canvas, cx, cy, radius, angle, color)
    end do

    call bitmap_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT)
    call bitmap_canva%save_to_file(radial_canvas, file_name,"png")

    call svg_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT)
    call svg_canva%save_to_file(radial_canvas, file_name)

    call bitmap_canva%destroy()
    call test_both(file_name,bitmap_canva)
contains

    subroutine draw_radial_line(canva, cx, cy, radius, angle, color)
        type(drawing), intent(inout) :: canva
        real, intent(in) :: cx, cy, radius
        real, intent(in) :: angle
        type(RGB), intent(in) :: color
        integer :: x1, y1, x2, y2
        real :: cos_angle, sin_angle

        cos_angle = cos(angle)
        sin_angle = sin(angle)
        sh%p1%x = (cx + int(radius * cos_angle)) / CANVAS_WIDTH
        sh%p1%y = (cy + int(radius * sin_angle)) / CANVAS_HEIGHT
        sh%p2%x = (cx - int(radius * cos_angle)) / CANVAS_WIDTH
        sh%p2%y = (cy - int(radius * sin_angle)) / CANVAS_HEIGHT
        sh%stroke_color = color_palette(mod(ind, 8) + 1)
        ind =ind +1

        call canva%add_shape(sh)
    end subroutine draw_radial_line

    subroutine random_color(color)
        type(RGB), intent(out) :: color
        real :: r, g, b
        call random_number(r)
        call random_number(g)
        call random_number(b)
        color%r = int(r * 255)
        color%g = int(g * 255)
        color%b = int(b * 255)
        color%a = 255
    end subroutine random_color

end program radial_lines

