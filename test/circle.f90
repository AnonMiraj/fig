program circle_test
    use fig_primitive
    use fig_canvas
    use fig_rgb
    implicit none

    type(canvas)  :: circle
    integer :: cx, cy, radius, result
    call canvas_init(circle, 400, 400, "green_blob")

    cx = circle%width / 2   ! Center x-coordinate
    cy = circle%height / 3  ! Center y-coordinate
    radius = min(circle%width, circle%height) / 8  ! Radius

    call fig_fill(circle, GREEN)

    call fig_fill_circle(circle, cx+cx/2, cy, radius, WHITE)
    call fig_fill_circle(circle, cx+cx/2, cy, radius/2, BLUE)
    call fig_fill_circle(circle, cx+cx/2, cy, radius/4, CYAN)

    call fig_fill_circle(circle, cx-cx/2, cy, radius, WHITE)
    call fig_fill_circle(circle, cx-cx/2, cy, radius/2, BLUE)
    call fig_fill_circle(circle, cx-cx/2, cy, radius/4, CYAN)


    call fig_fill_rect(circle, cx-cx/2, 2*cy,cx,cy/2, BLACK)

    call fig_save_to_ppm_file(circle, result)

    if (result == 0) then
        print *, 'Image successfully saved to green_blob.ppm'
    else
        print *, 'Error occurred while saving the image'
    end if

end program circle_test
