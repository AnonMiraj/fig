program smile

    use fig_canvas
    use fig_rgb
    use fig_primitive
    implicit none
    
    type(canvas) :: canva
    
    integer, parameter :: face_radius = 100
    integer, parameter :: eye_radius = 25
    integer, parameter :: mouth_radius_x = 50
    integer, parameter :: mouth_radius_y = 25
    integer :: face_center_x, face_center_y
    integer :: eye1_center_x, eye1_center_y
    integer :: eye2_center_x, eye2_center_y
    integer :: result
    
    call canvas_init(canva, 400, 400, "Smiley Face")
    
    face_center_x = canva%width / 2
    face_center_y = canva%height / 2
    eye1_center_x = face_center_x - face_radius / 2
    eye1_center_y = face_center_y - face_radius / 3
    eye2_center_x = face_center_x + face_radius / 2
    eye2_center_y = face_center_y - face_radius / 3
    
    call fig_fill_circle(canva, face_center_x, face_center_y, face_radius, YELLOW)
    
    call fig_fill_circle(canva, eye1_center_x, eye1_center_y, eye_radius, WHITE) 
    call fig_fill_circle(canva, eye2_center_x, eye2_center_y, eye_radius, WHITE)

    call fig_fill_circle(canva, eye1_center_x, eye1_center_y, eye_radius/2, BLACK)
    call fig_fill_circle(canva, eye2_center_x, eye2_center_y, eye_radius/2, BLACk)
    
    call fig_fill_ellipse(canva, face_center_x, face_center_y + face_radius / 3, mouth_radius_x, mouth_radius_y, RED)

    call fig_save_to_ppm_file(canva, result)
     
    if (result == 0) then
        print *, 'Image successfully saved to smiley_face.ppm'
    else
        print *, 'Error occurred while saving the image'
    end if

end program smile
