program test_fig_fill_triangle
    use fig_canvas
    use fig_primitive
    use fig_rgb
    implicit none

    integer :: result
    type(canvas) :: test_canvas
    type(RGB), dimension(0:6) :: colors
    integer :: i, j, ind=0,ind1,ind2
    integer :: triangle_size = 50

    call canvas_init(test_canvas, 801, 801, "cool_triangle_pattern")

    do i = 0, 6
        call random_color(colors(i))
    end do

    do i = 0, 800, triangle_size
        do j = 0, 800, triangle_size
            ind = mod(i + 2 * j, 7)
            ind1 = mod(ind * 2, 7)
            ind2 = mod(ind * 3, 7)

            call fig_fill_triangle(test_canvas, i, j, i + triangle_size, j, i + triangle_size, j + triangle_size, colors(ind1))
            call fig_fill_triangle(test_canvas, i, j, i, j + triangle_size, i + triangle_size, j + triangle_size, colors(ind2))

            call fig_draw_triangle(test_canvas, i, j, i + triangle_size, j, i + triangle_size, j + triangle_size, BLACK)
            call fig_draw_triangle(test_canvas, i, j, i, j + triangle_size, i + triangle_size, j + triangle_size, BLACK)

            call fig_fill_circle(test_canvas, i, j, 6, BLACK)
            call fig_fill_circle(test_canvas, i + triangle_size, j, 6, BLACK)
            call fig_fill_circle(test_canvas, i + triangle_size, j + triangle_size, 6, BLACK)
            call fig_fill_circle(test_canvas, i, j + triangle_size, 6, BLACK)
            call fig_fill_circle(test_canvas, i + triangle_size, j + triangle_size, 6, BLACK)
            call fig_fill_circle(test_canvas, i + triangle_size / 2, j + triangle_size / 2, 6, BLACK)
        
            call fig_fill_circle(test_canvas, i, j, 5, colors(ind1))
            call fig_fill_circle(test_canvas, i + triangle_size, j, 5, colors(ind1))
            call fig_fill_circle(test_canvas, i + triangle_size, j + triangle_size, 5, colors(ind1))
            call fig_fill_circle(test_canvas, i, j + triangle_size, 5, colors(ind2))
            call fig_fill_circle(test_canvas, i + triangle_size, j + triangle_size, 5, colors(ind2))
            call fig_fill_circle(test_canvas, i + triangle_size / 2, j + triangle_size / 2, 5, colors(ind))
        end do
    end do

    ! Save canvas to a PPM file
    call fig_save_to_ppm_file(test_canvas, result)

    if (result == 0) then
        print *, 'Image successfully saved to cool_triangle_pattern.ppm'

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
end program test_fig_fill_triangle

