module fig_primitive
    use fig_rgb
    use fig_canvas
    implicit none
    integer:: STROKE = 10

contains

    subroutine fig_fill(canva, background)
        implicit none
        type(canvas), intent(inout) :: canva
        type(RGB), intent(in) :: background
        integer :: color 
        color = rgb_to_int(background)

        canva%pixels = color

    end subroutine fig_fill


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
        integer :: color
        
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
        integer :: color
        integer :: x, y, x_end, y_end
        
        color = rgb_to_int(rgb_color)
        
        x_end = min(p%x + w - 1, canva%width)
        y_end = min(p%y + h - 1, canva%height)
        
        do y = max(p%y, 0), min(y_end, canva%height - 1)
            do x = max(p%x, 0), min(x_end, canva%width - 1)
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

    subroutine fig_draw_triangle(canva, x0, y0, x1, y1, x2, y2, rgb_color)
        type(canvas), intent(inout) :: canva
        integer, intent(in) :: x0, y0, x1, y1, x2, y2
        type(RGB), intent(in) :: rgb_color
    
        call fig_draw_line(canva,x0,y0,x1,y1,rgb_color)
        call fig_draw_line(canva,x1,y1,x2,y2,rgb_color)
        call fig_draw_line(canva,x2,y2,x0,y0,rgb_color)
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

subroutine fig_fill_triangle(canva, x0, y0, x1, y1, x2, y2, rgb_color)
    !! TODO relly need to clean this mess of a subroutine
    type(canvas), intent(inout) :: canva
    integer, intent(in) :: x0, y0, x1, y1, x2, y2
    type(RGB), intent(in) :: rgb_color
    integer :: xn0, yn0, xn1, yn1, xn2, yn2
    integer :: x, y
    integer :: s1, s2
    integer :: c01, c12
    integer :: dx01, dy01
    integer :: dx02, dy02
    integer :: dx21, dy21
    integer :: dx20, dy20
    integer :: color

    color = rgb_to_int(rgb_color)

    xn0 = x0
    yn0 = y0 
    xn1 = x1
    yn1 = y1
    xn2 = x2
    yn2 = y2

    ! Ensure yn0 <= yn1 <= yn2
    if (yn0 > yn1) then
        call swap_integers(xn0, xn1)
        call swap_integers(yn0, yn1)
    end if
    
    if (yn1 > yn2) then
        call swap_integers(xn1, xn2)
        call swap_integers(yn1, yn2)
    end if
    
    if (yn0 > yn1) then
        call swap_integers(xn0, xn1)
        call swap_integers(yn0, yn1)
    end if

    dy01 = yn1 - yn0
    dx01 = xn1 - xn0
    dy02 = yn2 - yn0
    dx02 = xn2 - xn1

    do y = max(yn0, 0), min(yn1, canva%height - 1), 1
        if (dy01 /= 0) then
            s1 = (y - yn0) * dx01 / dy01 + xn0
        else
            s1 = xn0
        end if

        if (dy02 /= 0) then
            s2 = (y - yn0) * dx02 / dy02 + xn0
        else
            s2 = xn0
        end if

        if (s1 > s2) then
            call swap_integers(s1, s2)
        end if

        do x = max(s1, 0), min(s2, canva%width - 1), 1
            canva%pixels(x, y) = color
        end do
    end do

    dy21 = yn1 - yn2
    dx21 = xn1 - xn2
    dy20 = yn0 - yn2
    dx20 = xn0 - xn2

    do y = max(yn1, 0), min(yn2, canva%height - 1), 1
        if (dy21 /= 0) then
            s1 = (y - yn2) * dx21 / dy21 + xn2
        else
            s1 = xn2
        end if

        if (dy20 /= 0) then
            s2 = (y - yn2) * dx20 / dy20 + xn2
        else
            s2 = xn2
        end if

        if (s1 > s2) then
            call swap_integers(s1, s2)
        end if

        do x = max(s1, 0), min(s2, canva%width - 1), 1
            canva%pixels(x, y) = color
        end do
    end do

end subroutine fig_fill_triangle


subroutine fig_fill_circle(canva, cx, cy, r, rgb_color)
    type(canvas), intent(inout) :: canva
    integer, intent(in) :: cx, cy, r
    type(RGB), intent(in) :: rgb_color
    integer :: color 
    integer :: x, y, dx, dy
    
    color = rgb_to_int(rgb_color)

    do y = max(cy - r, 0), min(cy + r, canva%height - 1)
        do x = max(cx - r, 0), min(cx + r, canva%width - 1)
            dx = x - cx
            dy = y - cy
            if (dx*dx + dy*dy <= r*r) then
                canva%pixels(x, y) = color
            end if
        end do
    end do
end subroutine fig_fill_circle




subroutine fig_fill_ellipse(canva, cx, cy, r1, r2, rgb_color)
    type(canvas), intent(inout) :: canva
    integer, intent(in) :: cx, cy 
    integer, intent(in) :: r1, r2
    type(RGB), intent(in) :: rgb_color
    integer :: color 
    integer :: x, y
    
    color = rgb_to_int(rgb_color)
    
    do y = max(cy - r2, 0), min(cy + r2, canva%height - 1)
        do x = max(cx - r1, 0), min(cx + r1, canva%width - 1)
            if (((x - cx) / real(r1))**2 + ((y - cy) / real(r2))**2 <= 1.0) then
                canva%pixels(x, y) = color
            end if
        end do
    end do
end subroutine fig_fill_ellipse
end module fig_primitive

