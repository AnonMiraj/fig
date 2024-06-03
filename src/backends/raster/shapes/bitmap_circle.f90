module fig_bitmap_circle
    use fig_shapes
    use fig_canvas
    use fig_bitmap_utils

contains

    subroutine write_circle(canva, pixels, circ)
        type(circle), intent(in) :: circ
        integer(pixel), dimension(:,:), intent(inout):: pixels
        class(base_canvas), intent(inout) :: canva

        call draw_inner_circle(canva, pixels, circ)
        call draw_outer_circle(canva, pixels, circ)
    end subroutine write_circle

    subroutine draw_inner_circle(canva, pixels,circ)
        type(circle), intent(in) :: circ
        integer(pixel), dimension(:,:), intent(inout):: pixels
        class(base_canvas), intent(inout) :: canva
        integer(pixel) :: fill_color
        integer :: x, y, d
        fill_color = rgb_to_int(circ%fill_color)
        x = 0
        y = int(circ%r)
        d = 1 - int(circ%r)

        do while (x <= y)
            if (d < 0) then
                d = d + 2 * x + 3
            else
                d = d + 2 * (x - y) + 5
                y = y - 1
            end if
            x = x + 1

            call fill_rect(canva, pixels, int(circ%cx) - x, int(circ%cy) + y - 1, 2 * x, 1, fill_color)
            call fill_rect(canva, pixels, int(circ%cx) - x, int(circ%cy) - y, 2 * x, 1, fill_color)
            call fill_rect(canva, pixels, int(circ%cx) - y, int(circ%cy) + x - 1, 2 * y, 1, fill_color)
            call fill_rect(canva, pixels, int(circ%cx) - y, int(circ%cy) - x, 2 * y, 1, fill_color)
        end do

        call fill_rect(canva, pixels, int(circ%cx) - int(circ%r), int(circ%cy), 2 * int(circ%r), 1, fill_color)
    end subroutine draw_inner_circle

    subroutine draw_outer_circle(canva, pixels, circ)
        type(circle), intent(in) :: circ
        integer(pixel), dimension(:,:), intent(inout):: pixels
        class(base_canvas), intent(inout) :: canva
        integer(pixel) :: stroke_color
        integer :: x, y, d

        stroke_color = rgb_to_int(circ%stroke_color)
        x = 0
        y = int(circ%r)
        d = 1 - int(circ%r)

        do while (x < y)
            if (d < 0) then
                d = d + 2 * x + 3
            else
                d = d + 2 * (x - y) + 5
                y = y - 1
            end if
            x = x + 1

            call draw_pixel(canva, pixels, int(circ%cx) + x, int(circ%cy) + y, stroke_color)
            call draw_pixel(canva, pixels, int(circ%cx) - x, int(circ%cy) + y, stroke_color)
            call draw_pixel(canva, pixels, int(circ%cx) + x, int(circ%cy) - y, stroke_color)
            call draw_pixel(canva, pixels, int(circ%cx) - x, int(circ%cy) - y, stroke_color)
            call draw_pixel(canva, pixels, int(circ%cx) + y, int(circ%cy) + x, stroke_color)
            call draw_pixel(canva, pixels, int(circ%cx) - y, int(circ%cy) + x, stroke_color)
            call draw_pixel(canva, pixels, int(circ%cx) + y, int(circ%cy) - x, stroke_color)
            call draw_pixel(canva, pixels, int(circ%cx) - y, int(circ%cy) - x, stroke_color)
        end do

        call draw_pixel(canva, pixels, int(circ%cx), int(circ%cy - circ%r), stroke_color)
        call draw_pixel(canva, pixels, int(circ%cx), int(circ%cy + circ%r), stroke_color)
        call draw_pixel(canva, pixels, int(circ%cx - circ%r), int(circ%cy), stroke_color)
        call draw_pixel(canva, pixels, int(circ%cx + circ%r), int(circ%cy), stroke_color)
    end subroutine draw_outer_circle

end module fig_bitmap_circle
