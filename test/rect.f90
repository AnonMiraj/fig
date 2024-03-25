program rect_test


    use fig_primitive
    use fig_canvas
    use fig_rgb
    implicit none

    type(canvas)  :: rect
    integer ::  result
    call canvas_init(rect, 800, 600, "rect")
    call fig_fill(rect, CYAN)

    call fig_fill_rect(rect, 400, 300,400, 300, RED)
    call fig_fill_rect(rect, 400, 0,400, 300, BLUE)

    call fig_save_to_ppm_file(rect, result)

    if (result == 0) then
        print *, 'Image successfully saved to rect.ppm'
    else
        print *, 'Error occurred while saving the image'
    end if
end program rect_test
