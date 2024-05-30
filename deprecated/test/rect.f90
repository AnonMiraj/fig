program rect_test


    use fig_primitive
    use fig_canvas
    use fig_rgb
    implicit none

    type(canvas)  :: rect
    integer ::  result
    call canvas_init(rect, 800, 600, "rect")
    call fig_fill(rect, CYAN)

    call fig_draw_rect(rect, 100, 100, 200, 150, RED)
    call fig_fill_rect(rect, 400, 100, 200, 150, BLUE)
    call fig_draw_rect(rect, 200, 300, 150, 200, GREEN)
    call fig_fill_rect(rect, 500, 300, 150, 200, YELLOW)
    call fig_draw_rect(rect, 100, 500, 250, 100, MAGENTA)
    call fig_fill_rect(rect, 400, 500, 250, 100, PINK)

    call fig_save_to_ppm_file(rect, result)
    if (result == 0) then
        print *, 'Image successfully saved to rect.ppm'
    else
        print *, 'Error occurred while saving the image'
    end if
end program rect_test
