module fig_bitmap_utils
    use fig_canvas
    implicit none
    
contains

    subroutine draw_pixel(canva,pixels, x, y, color)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(:,:), intent(inout):: pixels
        integer, intent(in) :: x, y
        integer(pixel), intent(in) :: color
    
        if (x >= 0 .and. x < int(canva%width) .and. y >= 0 .and. y < int(canva%height)) then
            pixels(x, y) = color
        end if
    end subroutine draw_pixel

    subroutine fill_rect(canva,pixels, x, y, w, h, color)
        class(base_canvas), intent(inout) :: canva
        integer(pixel), dimension(:,:), intent(inout):: pixels
        integer, intent(in) :: x, y
        integer, intent(in) :: w, h
        integer(pixel), intent(in) :: color
        integer :: i, j
        integer :: x_start, y_start
        integer :: x_end, y_end
        
        x_start = max(int(x),0)
        y_start = max(int(y),0)
        x_end = int(min(x + w, int(canva%width)))-1
        y_end = int(min(y + h, int(canva%height)))-1
        
        do i = y_start, y_end 
            do j = x_start, x_end 
                pixels(j, i) = color
            end do
        end do

    end subroutine fill_rect



    
end module fig_bitmap_utils

