module fig_primitive
    use fig_rgb
    use fig_canvas
    use fig_config
    implicit none

contains

    subroutine fig_fill(canva, background)
        implicit none
        type(canvas), intent(inout) :: canva
        type(RGB), intent(in) :: background
        integer(pixel) :: color 
        color = rgb_to_int(background)
        canva%pixels = color

    end subroutine fig_fill

    subroutine fig_draw_pixel_i(canva, x, y, color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: x, y
        integer(pixel), intent(in) :: color
    
        if (x >= 0 .and. x < canva%width .and. y >= 0 .and. y < canva%height) then
            canva%pixels(x, y) = color
        end if
    end subroutine fig_draw_pixel_i

    subroutine fig_draw_line(canva, x1, y1, x2, y2, rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: x1, y1, x2, y2
        type(vec2):: p1,p2
        type(RGB), intent(in) :: rgb_color
    
        p1= vec2(x1,y1)
        p2= vec2(x2,y2)
    
        call fig_draw_lineV(canva,p1,p2,rgb_color)
    
    end subroutine fig_draw_line

    subroutine fig_draw_lineV(canva, p1, p2, rgb_color)
        ! The Bresenham's line algorithm
        type(canvas), intent(inout) :: canva
        type(RGB), intent(in) :: rgb_color
        type(vec2), intent(in) :: p1,p2
        integer(pixel) :: color
        
        integer :: dx, dy, x, y
        integer :: sx, sy, err, e2
        
        color = rgb_to_int(rgb_color)
        
        dx = abs(p2%x - p1%x)
        dy = abs(p2%y - p1%y)
        
        sx = sign(1, p2%x - p1%x)
        sy = sign(1, p2%y - p1%y)
        
        err = dx - dy
        
        x = p1%x
        y = p1%y
        
        do while ((x /= p2%x .or. y /= p2%y) .and. &
                  (x >= 0 .and. x < canva%width) .and. &
                  (y >= 0 .and. y < canva%height))
            canva%pixels(x, y) = color 
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
    end subroutine fig_draw_lineV



    subroutine fig_fill_rect(canva, x1, y1, w, h, rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: x1, y1, w, h
        type(RGB), intent(in) :: rgb_color

        type(vec2) :: p

        p=vec2(x1,y1)

        call fig_fill_rectV(canva,p,w,h,rgb_color)

    end subroutine fig_fill_rect

    subroutine fig_fill_rectV(canva, p, w, h, rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) ::  w, h
        type(RGB), intent(in) :: rgb_color
        type(vec2), intent(in) :: p
        integer(pixel) :: color
        integer :: x, y, x_end, y_end
        
        color = rgb_to_int(rgb_color)
        
        x_end = min(p%x + w - 1, canva%width-1)
        y_end = min(p%y + h - 1, canva%height-1)
        
        do y = max(p%y, 0), y_end 
            do x = max(p%x, 0), x_end 
                canva%pixels(x, y) = color
            end do
        end do

    end subroutine fig_fill_rectV

    subroutine fig_draw_rect(canva, x1, y1, w, h, rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: x1, y1, w, h
        type(RGB), intent(in) :: rgb_color
        type(vec2) :: p

        p=vec2(x1,y1)

        call fig_draw_rectV(canva, p, w, h, rgb_color)
        
    end subroutine fig_draw_rect

    subroutine fig_draw_rectV(canva, p, w, h, rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: w, h
        type(RGB), intent(in) :: rgb_color
        type(vec2), intent(in) :: p
        
        call fig_draw_line(canva, p%x, p%y, p%x + w, p%y, rgb_color) ! Top line
        call fig_draw_line(canva, p%x, p%y, p%x, p%y + h, rgb_color) ! Left line
        call fig_draw_line(canva, p%x + w - 1, p%y + h - 1, p%x + w - 1, p%y, rgb_color) ! Right line
        call fig_draw_line(canva, p%x + w - 1, p%y + h - 1, p%x, p%y + h - 1, rgb_color) ! Bottom line
    end subroutine fig_draw_rectV

    subroutine fig_draw_triangle(canva, x1, y1, x2, y2, x3, y3, rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: x1, y1, x2, y2, x3, y3
        type(RGB), intent(in) :: rgb_color
        type(vec2) :: p1,p2,p3
        
        p1=vec2(x1,y1)
        p2=vec2(x2,y2)
        p3=vec2(x3,y3)

        call fig_draw_triangleV(canva,p1,p2,p3,rgb_color)
    end subroutine fig_draw_triangle

    subroutine fig_draw_triangleV(canva, p1, p2, p3, rgb_color)
        type(canvas), intent(inout) :: canva
        type(vec2), intent(in) :: p1, p2, p3
        type(RGB), intent(in) :: rgb_color

        call fig_draw_line(canva,p1%x,p1%y,p2%x,p2%y,rgb_color)
        call fig_draw_line(canva,p2%x,p2%y,p3%x,p3%y,rgb_color)
        call fig_draw_line(canva,p3%x,p3%y,p1%x,p1%y,rgb_color)
    end subroutine fig_draw_triangleV

    subroutine swap_integers(a, b)
        integer, intent(inout) :: a, b
        integer :: temp
        temp = a
        a = b
        b = temp
    end subroutine swap_integers

    subroutine fig_fill_triangle(canva, x1, y1, x2, y2, x3, y3, rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: x1, y1, x2, y2, x3, y3
        type(RGB), intent(in) :: rgb_color
        type(vec2) :: p1,p2,p3
        
        p1=vec2(x1,y1)
        p2=vec2(x2,y2)
        p3=vec2(x3,y3)

        call fig_fill_triangleV(canva,p1,p2,p3,rgb_color)
    end subroutine fig_fill_triangle


    subroutine fig_fill_triangleV(canva, p1,p2,p3, rgb_color)
        type(canvas), intent(inout) :: canva
        type(vec2) :: p1,p2,p3
        type(RGB), intent(in) :: rgb_color
        integer :: y
        integer :: x_start, x_end
        integer :: dx12, dy12
        integer :: dx13, dy13
        integer :: dx32, dy32
        integer :: dx31, dy31
        integer(pixel) :: color

        color = rgb_to_int(rgb_color)

        call sort_vertices(p1, p2, p3)

        dx12 = p2%x - p1%x
        dy12 = p2%y - p1%y
        dx13 = p3%x - p2%x
        dy13 = p3%y - p1%y

        ! Fill the top part of the triangle
        do y = max(p1%y, 0), min(p2%y, canva%height - 1), 1
            if (dy12 /= 0) then
                x_start = (y - p1%y) * dx12 / dy12 + p1%x
            else
                x_start = p1%x
            end if
    
            if (dy13 /= 0) then
                x_end = (y - p1%y) * dx13 / dy13 + p1%x
            else
                x_end = p1%x
            end if
    
            x_start=max(x_start, 0)
            x_end= min(x_end, canva%width - 1)
            call fig_draw_rect(canva,x_start,y,x_end-x_start,1,rgb_color)
        end do
    
        dx32 = p2%x - p3%x
        dy32 = p2%y - p3%y
        dx31 = p1%x - p3%x
        dy31 = p1%y - p3%y

        ! Fill the bottom part of the triangle   
        do y = max(p2%y, 0), min(p3%y, canva%height - 1), 1

            if (dy32 /= 0) then
                x_start = (y - p3%y) * dx32 / dy32 + p3%x
            else
                x_start = p3%x
            end if
    
            if (dy31 /= 0) then
                x_end = (y - p3%y) * dx31 / dy31 + p3%x
            else
                x_end = p3%x
            end if
    
            x_start=max(x_start, 0)
            x_end= min(x_end, canva%width - 1)
            call fig_draw_rect(canva,x_start,y,x_end-x_start,1,rgb_color)
        end do
    
    end subroutine fig_fill_triangleV

    subroutine sort_vertices(p1, p2, p3)
        type(vec2), intent(inout) :: p1, p2, p3
        type(vec2) :: temp
    
        if (p1%y > p2%y) then
            temp = p1
            p1 = p2
            p2 = temp
        end if
    
        if (p2%y > p3%y) then
            temp = p2
            p2 = p3
            p3 = temp
        end if
    
        if (p1%y > p2%y) then
            temp = p1
            p1 = p2
            p2 = temp
        end if
    end subroutine sort_vertices


    subroutine fig_draw_circle(canva, cx, cy, r, rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: cx, cy, r
        type(RGB), intent(in) :: rgb_color

        integer(pixel) :: color
        integer :: x, y, d

        color = rgb_to_int(rgb_color)
        x = 0
        y = r
        d = 1 - r

        do while (x < y)
            if (d < 0) then
                d = d + 2 * x + 3
            else
                d = d + 2 * (x - y) + 5
                y = y - 1
            end if
            x = x + 1

            call fig_draw_pixel_i(canva, cx + x, cy + y, color)
            call fig_draw_pixel_i(canva, cx - x, cy + y, color)
            call fig_draw_pixel_i(canva, cx + x, cy - y, color)
            call fig_draw_pixel_i(canva, cx - x, cy - y, color)
            call fig_draw_pixel_i(canva, cx + y, cy + x, color)
            call fig_draw_pixel_i(canva, cx - y, cy + x, color)
            call fig_draw_pixel_i(canva, cx + y, cy - x, color)
            call fig_draw_pixel_i(canva, cx - y, cy - x, color)
        end do

        call fig_draw_pixel_i(canva, cx , cy - r, color)
        call fig_draw_pixel_i(canva, cx , cy + r, color)
        call fig_draw_pixel_i(canva, cx - r, cy , color)
        call fig_draw_pixel_i(canva, cx + r, cy , color)
    end subroutine fig_draw_circle

    subroutine fig_draw_circleV(canva,center,r,rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: r
        type(vec2):: center
        type(RGB), intent(in) :: rgb_color
        call fig_draw_circle(canva,center%x,center%y,r,rgb_color)
    end subroutine fig_draw_circleV

    subroutine fig_fill_circle(canva, cx, cy, r, rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: cx, cy, r
        type(RGB), intent(in) :: rgb_color
        integer :: x, y
        integer :: d
        x = 0
        y = r
        d = 1 - r

        do while (x <= y)
            if (d < 0) then
                d = d + 2 * x + 3
            else
                d = d + 2 * (x - y) + 5
                y = y - 1
            end if
            x = x + 1


            call fig_fill_rect(canva, cx - x, cy + y, 2*x+1, 1, rgb_color)
            call fig_fill_rect(canva, cx - x, cy - y, 2*x+1, 1, rgb_color)
            call fig_fill_rect(canva, cx - y, cy + x, 2*y+1, 1, rgb_color)
            call fig_fill_rect(canva, cx - y, cy - x, 2*y+1, 1, rgb_color)   
        end do

        call fig_fill_rect(canva, cx - r, cy, 2*r+1, 1, rgb_color)

    end subroutine fig_fill_circle

    subroutine fig_fill_circleV(canva,center,r,rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: r
        type(vec2):: center
        type(RGB), intent(in) :: rgb_color
        call fig_fill_circle(canva,center%x,center%y,r,rgb_color)
    end subroutine fig_fill_circleV

    subroutine fig_draw_ellipse(canva, cx, cy, ra, rb, rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: cx, cy, ra, rb
        type(RGB), intent(in) :: rgb_color
        
        integer(pixel) :: color
        integer :: x, y, dx, dy, err, two_a_square, two_b_square, x_end, y_end

        color = rgb_to_int(rgb_color)

        two_a_square = 2 * ra * ra
        two_b_square = 2 * rb * rb
        x = ra
        y = 0
        dx = rb * rb * (1 - 2 * ra)
        dy = ra * ra
        err = 0
        x_end = two_b_square * ra
        y_end = 0

        do while (x_end >= y_end)
            call fig_draw_pixel_i(canva, cx + x, cy + y, color)
            call fig_draw_pixel_i(canva, cx - x, cy + y, color)
            call fig_draw_pixel_i(canva, cx + x, cy - y, color)
            call fig_draw_pixel_i(canva, cx - x, cy - y, color)
            
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
        y = rb
        dx = rb * rb
        dy = ra * ra * (1 - 2 * rb)
        err = 0
        x_end = 0
        y_end = two_a_square * rb
        do while (x_end <= y_end)
            call fig_draw_pixel_i(canva, cx + x, cy + y, color)
            call fig_draw_pixel_i(canva, cx - x, cy + y, color)
            call fig_draw_pixel_i(canva, cx + x, cy - y, color)
            call fig_draw_pixel_i(canva, cx - x, cy - y, color)
            
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
        end do
    End subroutine fig_draw_ellipse

    subroutine fig_draw_ellipseV(canva,center,ra,rb,rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: ra,rb
        type(vec2):: center
        type(RGB), intent(in) :: rgb_color
        call fig_draw_ellipse(canva,center%x,center%y,ra,rb,rgb_color)
    end subroutine fig_draw_ellipseV


    subroutine fig_fill_ellipse(canva, cx, cy, ra, rb, rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: cx, cy, ra, rb
        type(RGB), intent(in) :: rgb_color
        
        integer(pixel) :: color
        integer :: x, y, dx, dy, err, two_a_square, two_b_square, x_end, y_end

        color = rgb_to_int(rgb_color)

        two_a_square = 2 * ra * ra
        two_b_square = 2 * rb * rb
        x = ra
        y = 0
        dx = rb * rb * (1 - 2 * ra)
        dy = ra * ra
        err = 0
        x_end = two_b_square * ra
        y_end = 0

        do while (x_end >= y_end)
            
            call fig_fill_rect(canva, cx - x, cy + y, x*2+1,1, rgb_color)
            call fig_fill_rect(canva, cx - x, cy - y, x*2+1,1, rgb_color)

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
        y = rb
        dx = rb * rb
        dy = ra * ra * (1 - 2 * rb)
        err = 0
        x_end = 0
        y_end = two_a_square * rb
        do while (x_end <= y_end)
            call fig_fill_rect(canva, cx - x, cy + y, x*2+1,1, rgb_color)
            call fig_fill_rect(canva, cx - x, cy - y, x*2+1,1, rgb_color)

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
        end do
    End subroutine fig_fill_ellipse

    subroutine fig_fill_ellipseV(canva,center,ra,rb,rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: ra,rb
        type(vec2):: center
        type(RGB), intent(in) :: rgb_color
        call fig_fill_ellipse(canva,center%x,center%y,ra,rb,rgb_color)
    end subroutine fig_fill_ellipseV



    subroutine fig_fill_area(canva, cx, cy, rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: cx, cy 
        type(RGB), intent(in) :: rgb_color
        integer(pixel):: color, oldcolor
        integer:: x,y
        integer, dimension (:,:), allocatable :: pixel_queue
        logical, dimension(:,:) , allocatable :: pixel_mask
        integer:: start_queue
        integer:: end_queue
        integer:: queue_size
        
        color = rgb_to_int(rgb_color)

        if (cx < 0 .or. cx >= canva%width .or. cy < 0 .or. cy >= canva%height) &
             return
        
        ! currently naive memory management, should really be improved
        allocate (pixel_queue(1:2, canva%width*canva%height))
        allocate (pixel_mask(0:canva%width - 1, 0:canva%height - 1))

        pixel_mask = .false.
        
        !first pixel
        pixel_queue(1,1) = cx
        pixel_queue(2,1) = cy
        queue_size  = 1
        start_queue = 1
        end_queue   = 1

        oldcolor = canva%pixels(pixel_queue(1, start_queue) , pixel_queue(2, start_queue))

        do while (end_queue - start_queue >= 0 )

           ! process the first yet not processed item of the queue 
           canva%pixels(pixel_queue(1, start_queue) , pixel_queue(2, start_queue)) = color
           pixel_mask (pixel_queue(1, start_queue) , pixel_queue(2, start_queue)) = .true.

           ! test for pixel to the right
           if (pixel_queue(1, start_queue) < canva%width - 1) then
              if (canva%pixels(pixel_queue(1, start_queue) + 1, pixel_queue(2, start_queue)) == oldcolor &
                  .and. .not. pixel_mask(pixel_queue(1, start_queue) +1 , pixel_queue(2, start_queue) )) then
                 end_queue = end_queue + 1
                 pixel_queue(1, end_queue) = pixel_queue(1, start_queue) + 1
                 pixel_queue(2, end_queue) = pixel_queue(2, start_queue)
                 pixel_mask(pixel_queue(1, end_queue) , pixel_queue(2, end_queue) ) = .true. 
              endif
           end if

           ! test for pixel to the left
           if (pixel_queue(1, start_queue) > 0 ) then
              if (canva%pixels(pixel_queue(1, start_queue) - 1, pixel_queue(2, start_queue)) == oldcolor &
                  .and. .not. pixel_mask(pixel_queue(1, start_queue) -1 , pixel_queue(2, start_queue) )) then
                 end_queue = end_queue + 1
                 pixel_queue(1, end_queue) = pixel_queue(1, start_queue) - 1
                 pixel_queue(2, end_queue) = pixel_queue(2, start_queue)
                 pixel_mask(pixel_queue(1, end_queue) , pixel_queue(2, end_queue) ) = .true.                
              endif
           end if

           ! test for pixel below
           if (pixel_queue(2, start_queue) < canva%height - 1 ) then
              if (canva%pixels(pixel_queue(1, start_queue) , pixel_queue(2, start_queue) + 1) == oldcolor &
                  .and. .not. pixel_mask(pixel_queue(1, start_queue) , pixel_queue(2, start_queue) + 1 )) then
                 end_queue = end_queue + 1
                 pixel_queue(1, end_queue) = pixel_queue(1, start_queue)                 
                 pixel_queue(2, end_queue) = pixel_queue(2, start_queue) + 1
                 pixel_mask(pixel_queue(1, end_queue) , pixel_queue(2, end_queue) ) = .true. 
              endif
           end if

           ! test for pixel above
           if (pixel_queue(2, start_queue) > 0 ) then
              if (canva%pixels(pixel_queue(1, start_queue) , pixel_queue(2, start_queue) - 1) == oldcolor &
                  .and. .not. pixel_mask(pixel_queue(1, start_queue) , pixel_queue(2, start_queue) - 1 )) then
                 end_queue = end_queue + 1
                 pixel_queue(1, end_queue) = pixel_queue(1, start_queue)                                  
                 pixel_queue(2, end_queue) = pixel_queue(2, start_queue)  - 1
                 pixel_mask(pixel_queue(1, end_queue) , pixel_queue(2, end_queue) ) = .true. 
              endif
           end if

           start_queue = start_queue + 1
  
        end do
        
        deallocate( pixel_queue )
        deallocate( pixel_mask )
        
    end subroutine fig_fill_area

end module fig_primitive

