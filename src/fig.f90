
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
        integer :: i
        integer :: color 
        color = rgb_to_int(background)

        canva%pixels = color

    end subroutine fig_fill


subroutine fig_draw_line(canva, x1, y1, x2, y2, rgb_color)
    ! The Bresenham's line algorithm
    type(canvas), intent(inout) :: canva
    integer, intent(in) :: x1, y1, x2, y2
    type(RGB), intent(in) :: rgb_color
    integer :: color
    
    integer :: dx, dy, x, y
    integer :: sx, sy, err, e2
    
    color = rgb_to_int(rgb_color)
    
    dx = abs(x2 - x1)
    dy = abs(y2 - y1)
    
    sx = sign(1, x2 - x1)
    sy = sign(1, y2 - y1)
    
    err = dx - dy
    
    x = x1
    y = y1
    
    do while ((x /= x2 .or. y /= y2) .and. &
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
end subroutine fig_draw_line

subroutine fig_fill_rect(canva, x0, y0, w, h, rgb_color)
    type(canvas), intent(inout) :: canva
    integer, intent(in) :: x0, y0, w, h
    type(RGB), intent(in) :: rgb_color
    integer :: color
    integer :: x, y, x_end, y_end
    
    color = rgb_to_int(rgb_color)
    
    x_end = min(x0 + w - 1, canva%width)
    y_end = min(y0 + h - 1, canva%height)
    
    do y = max(y0, 0), min(y_end, canva%height - 1)
        do x = max(x0, 0), min(x_end, canva%width - 1)
            canva%pixels(x, y) = color
        end do
    end do
end subroutine fig_fill_rect

subroutine fig_draw_rect(canva, x0, y0, w, h, rgb_color)
    type(canvas), intent(inout) :: canva
    integer, intent(in) :: x0, y0, w, h
    type(RGB), intent(in) :: rgb_color
    
    call fig_draw_line(canva, x0, y0, x0 + w, y0, rgb_color) ! Top line
    call fig_draw_line(canva, x0, y0, x0, y0 + h, rgb_color) ! Left line
    call fig_draw_line(canva, x0 + w - 1, y0 + h - 1, x0 + w - 1, y0, rgb_color) ! Right line
    call fig_draw_line(canva, x0 + w - 1, y0 + h - 1, x0, y0 + h - 1, rgb_color) ! Bottom line
end subroutine fig_draw_rect

subroutine fig_draw_triangle(canva, x0, y0, x1, y1, x2, y2, rgb_color)
    type(canvas), intent(inout) :: canva
    integer, intent(in) :: x0, y0, x1, y1, x2, y2
    type(RGB), intent(in) :: rgb_color

    call fig_draw_line(canva,x0,y0,x1,y1,rgb_color)
    call fig_draw_line(canva,x1,y1,x2,y2,rgb_color)
    call fig_draw_line(canva,x2,y2,x0,y0,rgb_color)
end subroutine fig_draw_triangle

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
    dx02 = xn2 - xn0

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

