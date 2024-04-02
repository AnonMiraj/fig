

program test_fig_draw_triangle
    use fig_canvas
    use fig_primitive
    use fig_rgb
    implicit none

    integer :: result
    type(canvas) :: test_canvas
    type(RGB), dimension(0:6) :: colors
    integer :: i, j, ind=1
    integer :: triangle_size = 50

    ! Initialize canvas and colors
    call canvas_init(test_canvas, 801, 801, "test_triangle")

    ! Define colors in the array
    colors(0) = RED
    colors(1) = PINK
    colors(2) = YELLOW
    colors(3) = GREEN
    colors(4) = BLUE
    colors(5) = CYAN
    colors(6) = MAGENTA


    do i = 0, 801, triangle_size
        do j = 0, 801, triangle_size
            ind =mod(i+j,7)
            call fig_draw_triangle(test_canvas, i, j, i + triangle_size, j, i + triangle_size, j + triangle_size,colors(ind))
            call fig_draw_triangle(test_canvas, i, j, i, j + triangle_size, i + triangle_size, j + triangle_size,colors(ind))

        end do
    end do

    ! Save canvas to a PPM file
    call fig_save_to_ppm_file(test_canvas, result)

    if (result == 0) then
        print *, 'Image successfully saved to triangle.ppm'
    else
        print *, 'Error occurred while saving the image'
    end if
end program test_fig_draw_triangle


