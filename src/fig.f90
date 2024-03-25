
module fig_primitive
    use fig_rgb
    use fig_canvas
    implicit none

contains

    subroutine fig_save_to_ppm_file(canva, result)
        implicit none
        type(canvas), intent(inout) :: canva
        character(len=:), allocatable :: file_path
        integer, intent(out) :: result
        integer :: iunit, i
        integer :: bytes(3)

        file_path=canva%title // '.ppm'
        open(newunit=iunit, file=file_path, status='replace')

        write(iunit, '(a2)') 'P6'
        write(iunit, '(i0," ",i0)')  canva%width, canva%height
        write(iunit, '(i0)') 255

        do i = 0, canva%width*canva%height-1
            bytes(1) = ibits(canva%pixels(i),  0, 8)
            bytes(2) = ibits(canva%pixels(i),  8, 8)
            bytes(3) = ibits(canva%pixels(i), 16, 8)

            write(iunit,'(3a1)',advance='no') bytes
        end do

        close(iunit)
        result = 0
    end subroutine fig_save_to_ppm_file

    subroutine fig_fill(canva, background)
        implicit none
        type(canvas), intent(inout) :: canva
        type(RGB), intent(in) :: background
        integer :: i
        integer :: color 
        color = rgb_to_int(background)

        canva%pixels = color

    end subroutine fig_fill


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
            canva%pixels(y * canva%width + x) = color
        end do
    end do
end subroutine fig_fill_rect


subroutine fig_draw_line(canva, x1, y1, x2, y2, rgb_color)
    type(canvas), intent(inout) :: canva
    integer, intent(in) :: x1, y1, x2, y2
    type(RGB), intent(in) :: rgb_color
    integer :: color
    
    integer :: dx, dy, x, y, x_start, x_end, y_start, y_end
    
    color = rgb_to_int(rgb_color)
    
    ! Calculate deltas
    dx = x2 - x1
    dy = y2 - y1
    
    ! Determine start and end points
    x_start = max(min(x1, x2), 0)
    x_end = min(max(x1, x2), canva%width - 1)
    y_start = max(min(y1, y2), 0)
    y_end = min(max(y1, y2), canva%height - 1)
    
    ! Draw horizontal lines
    if (dy == 0) then
        do x = x_start, x_end
            if (x >= 0 .and. x < canva%width) then
                y = max(min(y1, canva%height - 1), 0)
                canva%pixels(y * canva%width + x) = color
            endif
        end do
    else
        ! Draw lines with non-zero slope
        do x = x_start, x_end
            y = (dy * (x - x1)) / dx + y1
            if (y >= 0 .and. y < canva%height) then
                canva%pixels(y * canva%width + x) = color
            endif
        end do
    endif
end subroutine fig_draw_line


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
                canva%pixels(y * canva%width + x) = color
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
                canva%pixels(y * canva%width + x) = color
            end if
        end do
    end do
end subroutine fig_fill_ellipse
end module fig_primitive

