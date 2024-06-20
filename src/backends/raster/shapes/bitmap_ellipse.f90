module fig_bitmap_ellipse
    use fig_shapes
    use fig_canvas
    use fig_bitmap_utils

contains

    subroutine write_ellipse(canva, pixels, ellips)
        type(ellipse), intent(in) :: ellips
        integer(pixel), dimension(:,:), intent(inout):: pixels
        class(base_canvas), intent(inout) :: canva

        call draw_inner_ellipse(canva, pixels, ellips)
        call draw_outer_ellipse(canva, pixels, ellips)
    end subroutine write_ellipse

    subroutine draw_outer_ellipse(canva, pixels,ellips)
        type(ellipse), intent(in) :: ellips
        integer(pixel), dimension(:,:), intent(inout):: pixels
        class(base_canvas), intent(inout) :: canva
        integer(pixel) :: stroke_color
        integer :: x, y, d
        integer :: dx, dy, err, two_a_square, two_b_square, x_end, y_end
        type(canvas_point) :: c
        c=to_canvas(ellips%center,canva%size)
        stroke_color = rgb_to_int(ellips%stroke_color)

        two_a_square = 2 * int(ellips%rx * ellips%rx)
        two_b_square = 2 * int(ellips%ry * ellips%ry)
        x = ellips%rx
        y = 0
        dx = ellips%ry * ellips%ry * (1 - 2 * ellips%rx)
        dy = ellips%rx * ellips%rx
        err = 0
        x_end = two_b_square * ellips%rx
        y_end = 0

        do while (x_end >= y_end)
            
            y = y + 1
            y_end = y_end + two_a_square
            err = err + dy
            dy = dy + two_a_square
            if ( (2 * err + dx) > 0) then
                x = x - 1
                x_end = x_end - two_b_square
                err = err + dx
                dx = dx + two_b_square
            end if

            call draw_pixel(canva,pixels, c%x + x, c%y + y, stroke_color)
            call draw_pixel(canva,pixels, c%x - x, c%y + y, stroke_color)
            call draw_pixel(canva,pixels, c%x + x, c%y - y, stroke_color)
            call draw_pixel(canva,pixels, c%x - x, c%y - y, stroke_color)
        end do

        x = 0
        y = ellips%ry
        dx = ellips%ry * ellips%ry
        dy = ellips%rx * ellips%rx * (1 - 2 * ellips%ry)
        err = 0
        x_end = 0
        y_end = two_a_square * ellips%ry
        do while (x_end <= y_end)
            
            x = x + 1
            x_end = x_end + two_b_square
            err = err + dx
            dx = dx + two_b_square
            if ( (2 * err + dy) > 0) then
                y = y - 1
                y_end = y_end - two_a_square
                err = err + dy
                dy = dy + two_a_square
            end if
            call draw_pixel(canva,pixels, c%x+ x, c%y+ y, stroke_color)
            call draw_pixel(canva,pixels, c%x- x, c%y+ y, stroke_color)
            call draw_pixel(canva,pixels, c%x+ x, c%y- y, stroke_color)
            call draw_pixel(canva,pixels, c%x- x, c%y- y, stroke_color)
        end do

        call draw_pixel(canva,pixels, int(c%x+ellips%rx), c%y, stroke_color)
        call draw_pixel(canva,pixels, c%x, int(c%y+ellips%ry), stroke_color)
        call draw_pixel(canva,pixels, int(c%x-ellips%rx), c%y, stroke_color)
        call draw_pixel(canva,pixels, c%x, int(c%y-ellips%ry), stroke_color)
    end subroutine draw_outer_ellipse


    subroutine draw_inner_ellipse(canva, pixels,ellips)
        type(ellipse), intent(in) :: ellips
        integer(pixel), dimension(:,:), intent(inout):: pixels
        class(base_canvas), intent(inout) :: canva
        integer(pixel) :: fill_color
        integer :: x, y, d,i
        integer :: dx, dy, err, two_a_square, two_b_square, x_end, y_end

        type(canvas_point) :: c
        c=to_canvas(ellips%center,canva%size)
        fill_color = rgb_to_int(ellips%fill_color)

        two_a_square = 2 * int(ellips%rx * ellips%rx)
        two_b_square = 2 * int(ellips%ry * ellips%ry)
        x = ellips%rx
        y = 0
        dx = ellips%ry * ellips%ry * (1 - 2 * ellips%rx)
        dy = ellips%rx * ellips%rx
        err = 0
        x_end = two_b_square * ellips%rx
        y_end = 0

        do while (x_end >= y_end)
            do i = c%x - x+1, c%x + x-1
                call draw_pixel(canva, pixels, i, c%y + y, fill_color)
                if (.not.( c%y .eq. c%y - y)) then
                    call draw_pixel(canva, pixels, i, c%y - y, fill_color)
                end if
            end do           
            y = y + 1
            y_end = y_end + two_a_square
            err = err + dy
            dy = dy + two_a_square
            if ( (2 * err + dx) > 0) then
                x = x - 1
                x_end = x_end - two_b_square
                err = err + dx
                dx = dx + two_b_square
            end if
        end do

        x = 0
        y = ellips%ry
        dx = ellips%ry * ellips%ry
        dy = ellips%rx * ellips%rx * (1 - 2 * ellips%ry)
        err = 0
        x_end = 0
        y_end = two_a_square * ellips%ry
        do while (x_end <= y_end)
            x = x + 1
            x_end = x_end + two_b_square
            err = err + dx
            dx = dx + two_b_square
            if ( (2 * err + dy) > 0) then
                do i = c%x - x+1, c%x + x-1
                    call draw_pixel(canva, pixels, i, c%y + y, fill_color)
                    call draw_pixel(canva, pixels, i, c%y - y, fill_color)
                end do           
                y = y - 1
                y_end = y_end - two_a_square
                err = err + dy
                dy = dy + two_a_square
            end if
        end do
    end subroutine draw_inner_ellipse

end module fig_bitmap_ellipse
