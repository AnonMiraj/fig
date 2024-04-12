program test_fig_fill_area
    use fig_canvas
    use fig_primitive
    use fig_rgb
    implicit none

    integer :: result
    type(canvas) :: test_canvas
    integer :: angle_counter
    integer, parameter:: MAX_ANGLES = 10000
    real :: pi = 4.0 * atan(1.0)

    call canvas_init(test_canvas, 401, 401, "fill_area")
    call fig_fill(test_canvas, BLACK)

    do angle_counter = 1, MAX_ANGLES
       
       call fig_draw_line(test_canvas,&
                          200 + nint(180*cos((2*pi/MAX_ANGLES)*(angle_counter-1))**3),&
                          200 + nint(180*sin((2*pi/MAX_ANGLES)*(angle_counter-1))**7),&
                          200 + nint(180*cos((2*pi/MAX_ANGLES)*angle_counter)**3),&
                          200 + nint(180*sin((2*pi/MAX_ANGLES)*angle_counter)**7),&
                          WHITE)
    enddo

    call fig_draw_circle(test_canvas, 200, 200, 30, red)

    call fig_fill_area(test_canvas, 200, 100, YELLOW)

    call fig_fill_area(test_canvas, 200, 200, RED)
   
    call fig_save_to_ppm_file(test_canvas, result)
    
    if (result == 0) then
        print *, 'Image successfully saved to fill_area.ppm'
    else
        print *, 'Error occurred while saving the image'
    end if

end program test_fig_fill_area

