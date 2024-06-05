module fig_bitmap_utils
    use fig_canvas
    use fig_rgb
    implicit none
    
contains


    subroutine draw_pixel(canva,pixels, x, y, color)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(0:,0:), intent(inout):: pixels
        integer, intent(in) :: x, y
        integer(pixel), intent(in) :: color
    
        if (x >= 0 .and. x < int(canva%width) .and. y >= 0 .and. y < int(canva%height)) then
            pixels(x, y) = blend_color(pixels(x,y),color)
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
                pixels(j, i) =  blend_color(pixels(j,i),color)
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
            pixels(x, y) =  blend_color(pixels(x,y),color)
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
 


    subroutine fill_triangle(canva, pixels, x1, y1, x2, y2, x3, y3, color)
        !! TODO needs to organize this a bit
        class(base_canvas), intent(inout) :: canva
        integer, dimension(0:,0:), intent(inout) :: pixels
        integer, intent(in) :: x1, y1, x2, y2, x3, y3, color
        integer :: p1(2), p2(2), p3(2)
        integer :: x, y
        integer :: x_start, x_end
        integer :: dx12, dy12
        integer :: dx13, dy13
        integer :: dx32, dy32
        integer :: dx31, dy31
        p1(1) = x1; p1(2) = y1
        p2(1) = x2; p2(2) = y2
        p3(1) = x3; p3(2) = y3

        call sort_vertices(p1, p2, p3)
        dx12 = p2(1) - p1(1)
        dy12 = p2(2) - p1(2)
        dx13 = p3(1) - p1(1)
        dy13 = p3(2) - p1(2)

        ! Fill the top part of the triangle
        do y = max(p1(2), 0), min(p2(2), int(canva%height) - 1), 1
            if (dy12 /= 0) then
                x_start = (y - p1(2)) * dx12 / dy12 + p1(1)
            else
                x_start = p1(1)
            end if

            if (dy13 /= 0) then
                x_end = (y - p1(2)) * dx13 / dy13 + p1(1)
            else
                x_end = p1(1)
            end if

            if (x_start>x_end) call swap_integers(x_start,x_end)
            x_start = max(x_start, 0)
            x_end = min(x_end, int(canva%width) - 1)

            do x = x_start, x_end, 1
                pixels(x,y)= blend_color(pixels(x,y),color)
            end do
        end do

        dx32 = p2(1) - p3(1)
        dy32 = p2(2) - p3(2)
        dx31 = p1(1) - p3(1)
        dy31 = p1(2) - p3(2)

        ! Fill the bottom part of the triangle   
        do y = max(p2(2), 0), min(p3(2), int(canva%height) - 1), 1
            if (dy32 /= 0) then
                x_start = (y - p3(2)) * dx32 / dy32 + p3(1)
            else
                x_start = p3(1)
            end if

            if (dy31 /= 0) then
                x_end = (y - p3(2)) * dx31 / dy31 + p3(1)
            else
                x_end = p3(1)
            end if

            if (x_start>x_end) call swap_integers(x_start,x_end)
            x_start = max(x_start, 0)
            x_end = min(x_end, int(canva%width) - 1)
            do x = x_start, x_end, 1
                pixels(x,y)= blend_color(pixels(x,y),color)
            end do
                
        end do
    end subroutine fill_triangle

    subroutine swap_integers(a, b)
        integer, intent(inout) :: a, b
        integer :: temp
        temp = a
        a = b
        b = temp
    end subroutine swap_integers

    subroutine sort_vertices(p1, p2, p3)
        integer, intent(inout) :: p1(2), p2(2), p3(2)

        integer :: temp(2)
        if (p1(2) > p2(2)) then
            temp = p1
            p1 = p2
            p2 = temp
        end if

        if (p2(2) > p3(2)) then
            temp = p2
            p2 = p3
            p3 = temp
        end if

        if (p1(2) > p2(2)) then
            temp = p1
            p1 = p2
            p2 = temp
        end if 

    end subroutine sort_vertices
 
    subroutine blend_pixel(pixels, x, y, color)
        integer(pixel), dimension(0:,0:), intent(inout) :: pixels
        integer, intent(in) :: x, y, color

        pixels(x, y) = blend_color(pixels(x, y), color)
    end subroutine blend_pixel  
end module fig_bitmap_utils

