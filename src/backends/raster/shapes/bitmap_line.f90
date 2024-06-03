module fig_bitmap_line
    use fig_shapes
    use fig_canvas
    use fig_bitmap_utils

contains

    subroutine write_line(canva, pixels, l)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(0:,0:), intent(inout):: pixels
        type(line), intent(in) :: l
        integer(pixel) :: color
        
        integer :: dx, dy, x, y
        integer :: sx, sy, err, e2
        
        color = rgb_to_int(l%stroke_color)
        
        dx = abs(int(l%x2 - l%x1))
        dy = abs(int(l%y2 - l%y1))
        
        sx = sign(1,int(l%x2 - l%x1))
        sy = sign(1,int(l%y2 - l%y1))
        
        err = dx - dy
        
        x = l%x1
        y = l%y1
        
        do while ((x /= int(l%x2) .or. y /= int(l%y2)) .and. &
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
    end subroutine write_line
    

end module fig_bitmap_line
