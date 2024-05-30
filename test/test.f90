
program test_fig_draw_triangle
    use fig_canvas
    use fig_primitive
    use fig_rgb
    use fig_rgb_color_constants
    implicit none

    integer :: result
    type(canvas) :: test_canvas
    type(RGB), dimension(0:6) :: colors
    integer :: i, j, ind=0,ind1,ind2
    integer :: triangle_size = 50

    call canvas_init(test_canvas, 20, 20, "test")

    call fig_draw_line(test_canvas , 1 ,2 ,4 ,7 ,FIG_COLOR_LIGHTYELLOW)
   ! Save canvas to a PPM file
    call fig_save_to_ppm_file(test_canvas, result)

    if (result == 0) then
        print *, 'Image successfully saved to test.ppm'

    else
        print *, 'Error occurred while saving the image'
    end if

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
    end subroutine random_color
end program test_fig_draw_triangle

