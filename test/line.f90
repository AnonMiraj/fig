program radial_lines
    use fig_drawing
    use fig_shapes
    use fig_rgb_color_constants
    use fig_rgb
    implicit none

    integer :: result
    type(drawing) :: radial_canvas
    integer :: i
    real :: angle_step, angle
    integer :: cx, cy, radius
    type(RGB) :: color
    type(line) :: sh
    integer, parameter :: num_lines = 180

    call radial_canvas%init(800.0, 600.0, "Radial Lines")

    call radial_canvas%set_background(FIG_COLOR_BLACK)

    cx = int(radial_canvas%width / 2)
    cy = int(radial_canvas%height / 2)
    radius = 300

    angle_step = 1.0 * atan(1.0) / num_lines

    do i = 0, num_lines - 1
        call random_color(color)
        call draw_radial_line(radial_canvas, cx, cy, radius, 4*i*angle_step, color)
    end do


    call radial_canvas%save_to_file('svg')
    call radial_canvas%save_to_file('ppm')

    print *, "drawing exported successfully: radial_canvas.(ppm\svg)"
contains

    subroutine draw_radial_line(canva, cx, cy, radius, angle, color)
        type(drawing), intent(inout) :: canva
        integer, intent(in) :: cx, cy, radius
        real, intent(in) :: angle
        type(RGB), intent(in) :: color
        integer :: x1, y1, x2, y2
        real :: cos_angle, sin_angle

        cos_angle = cos(angle)
        sin_angle = sin(angle)
        sh%x1 = cx + int(radius * cos_angle)
        sh%y1 = cy + int(radius * sin_angle)
        sh%x2 = cx - int(radius * cos_angle)
        sh%y2 = cy - int(radius * sin_angle)
        sh%stroke_color=color

        call radial_canvas%add_shape(sh)
        
    end subroutine draw_radial_line

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
end program radial_lines

