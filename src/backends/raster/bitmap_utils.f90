module fig_bitmap_utils
    use fig_canvas
    implicit none
    
contains

    subroutine draw_pixel(canva,pixels, x, y, color)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(0:,0:), intent(inout):: pixels
        integer, intent(in) :: x, y
        integer(pixel), intent(in) :: color
    
        if (x >= 0 .and. x < int(canva%width) .and. y >= 0 .and. y < int(canva%height)) then
            pixels(x, y) = color
        end if
    end subroutine draw_pixel

    subroutine fill_rect(canva,pixels, x, y, w, h, color)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(0:,0:), intent(inout):: pixels
        integer, intent(in) :: x, y
        integer, intent(in) :: w, h
        integer(pixel), intent(in) :: color
        integer :: i, j
        integer :: x_start, y_start
        integer :: x_end, y_end
        
        x_start = max(int(x),0)
        y_start = max(int(y),0)
        x_end = min(x + w, int(canva%width)-1)
        y_end = min(y + h, int(canva%height)-1)
        
        do i = y_start, y_end 
            do j = x_start, x_end 
                pixels(j, i) = color
            end do
        end do

    end subroutine fill_rect

    subroutine draw_line(canva, pixels, x1,y1,x2,y2,color)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(0:,0:), intent(inout):: pixels
        integer(pixel), intent(in) :: color

        real, intent(in) :: x1, y1, x2, y2
        integer :: dx, dy, x, y
        integer :: sx, sy, err, e2
        
        dx = abs(int(x2 - x1))
        dy = abs(int(y2 - y1))
        
        sx = sign(1,int(x2 - x1))
        sy = sign(1,int(y2 - y1))
        
        err = dx - dy
        
        x = x1
        y = y1
        
        do while ((x /= int(x2) .or. y /= int(y2)) .and. &
                  (x >= 0 .and. x < int(canva%width)) .and. &
                  (y >= 0 .and. y < int(canva%height)))
            pixels(x, y) = color 
            e2 = 2 * err
            if (e2 > -dy) then
                err = err - dy
                x = x + sx
            end if
            if (e2 < dx) then
                err = err + dx
                y = y + sy
            end if
        end do
    end subroutine draw_line
 


    
end module fig_bitmap_utils

