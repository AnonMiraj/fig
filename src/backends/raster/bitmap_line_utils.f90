module fig_bitmap_line_utils
    use fig_canvas
    use fig_rgb
    use fig_bitmap_utils
    use fig_rgb_color_constants
    implicit none
    
    integer :: overlap_none = 0
    integer :: overlap_major = 1

contains

    subroutine draw_line_overlap(canva, pixels,x1, y1, x2, y2, overlap, color)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(:,:), intent(inout):: pixels
        integer(pixel), intent(in) :: color
        integer, intent(in)  :: x1, y1, x2, y2
        integer, intent(in) :: overlap
        integer :: x_start, y_start,x_end, y_end
        integer :: dx, dy,err, sx, sy

        x_start=x1
        y_start=y1
        x_end=x2
        y_end=y2

        x_start = min(x_start,canva%size%width - 1)
        x_end = min(x_end,canva%size%width - 1)
        x_start = max(x_start,0)
        x_end = max(x_end,0)
        y_start = min(y_start,canva%size%height - 1)
        y_end = min(y_end,canva%size%height - 1)
        y_start = max(y_start,0)
        y_end = max(y_end,0)

        if ((x_start == x_end) .or. (y_start == y_end)) then
            call draw_line(canva, pixels,x_start, y_start, x_end, y_end, color)
        else
            dx = x_end - x_start
            dy = y_end - y_start
            if (dx < 0) then
                dx = -dx
                sx = -1
            else
                sx = 1
            end if
            if (dy < 0) then
                dy = -dy
                sy = -1
            else
                sy = 1
            end if

            call blend_pixel(pixels,x_start, y_start, color)
            if (dx > dy) then
                err = dy*2 - dx
                do while (x_start /= x_end)
                    x_start = x_start + sx
                    if (err >= 0) then
                        if (overlap==overlap_major  ) then
                            call blend_pixel(pixels,x_start, y_start, color)
                        end if
                        y_start = y_start + sy
                        err = err - dx * 2
                    end if
                    err = err + dy*2
                    call blend_pixel(pixels,x_start, y_start, color)
                end do
            else
                err = dx * 2 - dy
                do while (y_start /= y_end)
                    y_start = y_start + sy
                    if (err >= 0) then
                        if (overlap==overlap_major) then
                            call blend_pixel(pixels,x_start, y_start, color)
                        end if
                        x_start = x_start + sx
                        err = err - dy*2
                    end if
                    err = err + dx * 2
                    call blend_pixel(pixels,x_start, y_start, color)
                end do
            end if
        end if

    end subroutine draw_line_overlap

    subroutine draw_thick_line(canva, pixels,x1,y1,x2,y2, width,aColor)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(:,:), intent(inout):: pixels
        integer(pixel), intent(in) :: aColor
        integer, intent(in) :: x1,y1,x2,y2
        integer :: x_start, y_start, x_end, y_end, width
        integer :: i, dx, dy,draw_start_count,err, sx, sy
        integer :: overlap
        logical :: swap

        x_start=x1
        y_start=y1
        x_end=x2
        y_end=y2

        x_start = min(x_start,canva%size%width - 1)
        x_end = min(x_end,canva%size%width - 1)
        x_start = max(x_start,0)
        x_end = max(x_end,0)
        y_start = min(y_start,canva%size%height - 1)
        y_end = min(y_end,canva%size%height - 1)
        y_start = max(y_start,0)
        y_end = max(y_end,0)


        if (width <= 1) then
            call draw_line(canva,pixels, x_start, y_start, x_end, y_end, aColor)
            return
        end if


        dy = x_end - x_start
        dx = y_end - y_start
        swap = .true.
        if (dx < 0) then
            dx = -dx
            sx = -1
            swap = .not. swap
        else
            sx = 1
        end if

        if (dy < 0) then
            dy = -dy
            sy = -1
            swap = .not. swap
        else
            sy = 1
        end if

        draw_start_count = width / 2

        if (dx >= dy) then
            if (swap) then
                draw_start_count = (width - 1) - draw_start_count
                sy = -sy
            else
                sx = -sx
            end if
            
            err = dy*2 - dx
            do i = draw_start_count, 1, -1
                x_start = x_start - sx
                x_end = x_end - sx
                if (err >= 0) then
                    y_start = y_start - sy
                    y_end = y_end - sy
                    err = err - dx*2
                end if
                err = err + dy*2
            end do
            
            call draw_line(canva,pixels, x_start, y_start, x_end, y_end, aColor)
            
            err = dy*2 - dx
            do i = width, 2, -1
                x_start = x_start + sx
                x_end = x_end + sx
                overlap = overlap_none
                if (err >= 0) then
                    y_start = y_start + sy
                    y_end = y_end + sy
                    err = err - dx*2
                    overlap = overlap_major
                end if
                err = err + dy*2
                call draw_line_overlap(canva,pixels,x_start, y_start, x_end, y_end, overlap, aColor)
            end do
        else
            if (swap) then
                sx = -sx
            else
                draw_start_count = (width - 1) - draw_start_count
                sy = -sy
            end if
            
            err = dx*2 - dy
            do i = draw_start_count, 1, -1
                y_start = y_start - sy
                y_end = y_end - sy
                if (err >= 0) then
                    x_start = x_start - sx
                    x_end = x_end - sx
                    err = err - dy*2
                end if
                err = err + dx*2
            end do
            
            call draw_line(canva,pixels, x_start, y_start, x_end, y_end, aColor)
            
            err = dx*2 - dy
            do i = width, 2, -1
                y_start = y_start + sy
                y_end = y_end + sy
                overlap = overlap_none
                if (err >= 0) then
                    x_start = x_start + sx
                    x_end = x_end + sx
                    err = err - dy*2
                    overlap = overlap_major
                end if
                err = err + dx*2
                call draw_line_overlap(canva,pixels,x_start, y_start, x_end, y_end, overlap, aColor)
            end do
        end if
    end subroutine draw_thick_line 

end module fig_bitmap_line_utils
