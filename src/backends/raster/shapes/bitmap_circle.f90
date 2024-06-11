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

    subroutine draw_outer_circle(canva, pixels, circ)
        type(circle), intent(in) :: circ
        integer(pixel), dimension(:,:), intent(inout):: pixels
        class(base_canvas), intent(inout) :: canva
        integer(pixel) :: stroke_color
        integer :: x, y, d
        type(canvas_point) :: c
        c=to_canvas(circ%center,canva%size)

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

            call draw_pixel(canva, pixels, c%x + x, c%y + y, stroke_color)
            call draw_pixel(canva, pixels, c%x - x, c%y + y, stroke_color)
            call draw_pixel(canva, pixels, c%x + x, c%y - y, stroke_color)
            call draw_pixel(canva, pixels, c%x - x, c%y - y, stroke_color)
            call draw_pixel(canva, pixels, c%x + y, c%y + x, stroke_color)
            call draw_pixel(canva, pixels, c%x - y, c%y + x, stroke_color)
            call draw_pixel(canva, pixels, c%x + y, c%y - x, stroke_color)
            call draw_pixel(canva, pixels, c%x - y, c%y - x, stroke_color)
        end do

        call draw_pixel(canva, pixels, c%x, c%y - int(circ%r), stroke_color)
        call draw_pixel(canva, pixels, c%x, c%y + int(circ%r), stroke_color)
        call draw_pixel(canva, pixels, c%x - int(circ%r), int(c%y), stroke_color)
        call draw_pixel(canva, pixels, c%x + int(circ%r), int(c%y), stroke_color)
    end subroutine draw_outer_circle

    subroutine draw_inner_circle(canva, pixels,circ)
        type(circle), intent(in) :: circ
        integer(pixel), dimension(:,:), intent(inout):: pixels
        class(base_canvas), intent(inout) :: canva
        integer(pixel) :: fill_color
        integer :: x, y, d, i
        type(canvas_point) :: c
        c=to_canvas(circ%center,canva%size)

        fill_color = rgb_to_int(circ%fill_color)

        x = 0
        y = int(circ%r)
        d = 1 - int(circ%r)

        do while (x <= y)

            do i = c%x - y, c%x + y
                call draw_pixel(canva,pixels, i, c%y + x, fill_color)
                if (.not.( i .eq. (c%y - x))) then
                    call draw_pixel(canva,pixels, i, c%y - x, fill_color)
                end if
            end do

            if (d < 0) then
                d = d + 2 * x + 3
            else
                do i = c%x - x, c%x + x
                    call draw_pixel(canva,pixels, i, c%y + y, fill_color)
                    call draw_pixel(canva,pixels, i, c%y - y, fill_color)
                end do
                d = d + 2 * (x - y) + 5
                y = y - 1
            end if
            x = x + 1

        end do

    end subroutine draw_inner_circle

end module fig_bitmap_circle

