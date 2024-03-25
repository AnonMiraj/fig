
program radial_lines
    use fig_primitive
    use fig_canvas
    use fig_rgb
    implicit none

    integer :: result
    type(canvas) :: radial_canvas
    integer :: i
    real :: angle_step, angle
    integer :: cx, cy, radius
    type(RGB) :: color
    integer, parameter :: num_lines = 50

    call canvas_init(radial_canvas, 800, 600, "Radial Lines")

    call fig_fill(radial_canvas, BLACK)

    cx = radial_canvas%width / 2
    cy = radial_canvas%height / 2
    radius = 360

    angle_step = 1 * atan(1.0) / num_lines

    do i = 0, num_lines - 1
        call random_color(color)
        call draw_radial_line(radial_canvas, cx, cy, radius, real(i)*0.75, color)
    end do

    ! Save canvas to PPM file
    call fig_save_to_ppm_file(radial_canvas, result)

    ! Check if saving was successful
    if (result == 0) then
        print *, 'Image successfully saved to radial_lines.ppm'
    else
        print *, 'Error occurred while saving the image'
    end if

contains

    ! Subroutine to draw a radial line from the center
    subroutine draw_radial_line(canva, cx, cy, radius, angle, color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: cx, cy, radius
        real, intent(in) :: angle
        type(RGB), intent(in) :: color
        integer :: x1, y1, x2, y2
        real :: cos_angle, sin_angle

        ! Calculate the coordinates of the start and end points of the line
        cos_angle = cos(angle)
        sin_angle = sin(angle)
        x1 = cx + int(radius * cos_angle)
        y1 = cy + int(radius * sin_angle)
        x2 = cx - int(radius * cos_angle)
        y2 = cy - int(radius * sin_angle)

        ! Draw the line on the canvas
        call fig_draw_line(canva, x1, y1, x2, y2, color)
    end subroutine draw_radial_line

    ! Subroutine to generate a random RGB color
    subroutine random_color(color)
        type(RGB) :: color
        real :: r, g, b
        call random_number(r)
        call random_number(g)
        call random_number(b)
        color%red = int(r * 255)
        color%green = int(g * 255)
        color%blue = int(b * 255)
    end subroutine random_color

end program radial_lines

