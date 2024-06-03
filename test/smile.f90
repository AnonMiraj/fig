program smile

    use fig_canvas
    use fig_shapes
    use fig_rgb_color_constants
    use fig_drawing
    use fig_rgb
    use fig_primitive
    implicit none
    
    type(drawing) :: canva
    
    integer, parameter :: face_radius = 100
    integer, parameter :: eye_radius = 25
    integer, parameter :: mouth_radius_x = 50
    integer, parameter :: mouth_radius_y = 25
    integer :: face_center_x, face_center_y
    integer :: eye1_center_x, eye1_center_y
    integer :: eye2_center_x, eye2_center_y
    integer :: result
    type(circle):: circ
    type(ellipse):: ellip
    
    call canva%init(400.0, 400.0, "Smiley Face")
    
    face_center_x = canva%width / 2
    face_center_y = canva%height / 2
    eye1_center_x = face_center_x - face_radius / 2
    eye1_center_y = face_center_y - face_radius / 3
    eye2_center_x = face_center_x + face_radius / 2
    eye2_center_y = face_center_y - face_radius / 3
    
    circ%cx=face_center_x
    circ%cy=face_center_y
    circ%r=face_radius
    circ%stroke_color=FIG_COLOR_BLACK
    circ%fill_color=FIG_COLOR_YELLOW
    call canva%add_shape(circ)

    
    circ%cx=eye1_center_x
    circ%cy=eye1_center_y
    circ%r=eye_radius
    circ%stroke_color=FIG_COLOR_WHITE
    circ%fill_color=FIG_COLOR_WHITE
    call canva%add_shape(circ)

    circ%r=eye_radius/2
    circ%stroke_color=FIG_COLOR_WHITE
    circ%fill_color=FIG_COLOR_BLACK
    call canva%add_shape(circ)



    circ%cx=eye2_center_x
    circ%cy=eye2_center_y
    circ%r=eye_radius
    circ%stroke_color=FIG_COLOR_WHITE
    circ%fill_color=FIG_COLOR_WHITE
    call canva%add_shape(circ)

    circ%r=eye_radius/2
    circ%stroke_color=FIG_COLOR_WHITE
    circ%fill_color=FIG_COLOR_BLACK
    call canva%add_shape(circ)

   
    ellip%cx=face_center_x
    ellip%cy=face_center_y+ face_radius / 3
    ellip%ra=mouth_radius_x
    ellip%rb=mouth_radius_y
    ellip%fill_color=FIG_COLOR_RED
    ellip%stroke_color=FIG_COLOR_RED
    call canva%add_shape(ellip)

    call canva%save_to_file('ppm')
    call canva%save_to_file('svg')

    print *, "drawing exported successfully: Smiley Face.(ppm\svg)"

     
    
end program smile
