program circle_test
    use fig_primitive
    use fig_canvas
    use fig_rgb
    implicit none

    type(canvas)  :: c
    integer :: cx, cy, radius, result
    call canvas_init(c, 400, 400, "green_blob")

    cx = c%width / 2 
    cy = c%height / 3
    radius = min(c%width, c%height) / 8 

    call fig_fill(c, GREEN)

    call fig_fill_circle(c, cx+cx/2, cy, radius, WHITE)
    call fig_fill_circle(c, cx+cx/2, cy, radius/2, BLUE)
    call fig_fill_circle(c, cx+cx/2, cy, radius/4, CYAN)

    call fig_fill_circle(c, cx-cx/2, cy, radius, WHITE)
    call fig_fill_circle(c, cx-cx/2, cy, radius/2, BLUE)
    call fig_fill_circle(c, cx-cx/2, cy, radius/4, CYAN)


    call fig_fill_rect(c, cx-cx/2, 2*cy,cx,cy/2, BLACK)

    call fig_save_to_ppm_file(c, result)

    if (result == 0) then
        print *, 'Image successfully saved to green_blob.ppm'
    else
        print *, 'Error occurred while saving the image'
    end if

end program circle_test
