
program smile
    use fig_drawing
    use fig_shapes
    use fig_rgb_color_constants
    use fig_rgb
    use fig_svg
    use fig_bitmap
    use fig_test
    implicit none

    integer, parameter :: CANVAS_WIDTH = 400
    integer, parameter :: CANVAS_HEIGHT = 400
    integer, parameter :: FACE_RADIUS = 100
    integer, parameter :: EYE_RADIUS = 25
    integer, parameter :: MOUTH_RADIUS_X = 50
    integer, parameter :: MOUTH_RADIUS_Y = 25
    character(len=:), allocatable  :: file_name

    type(drawing) :: canva
    type(circle) :: circ
    type(ellipse) :: ellip
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva

    real:: face_center_x, face_center_y
    real:: eye1_center_x, eye1_center_y
    real:: eye2_center_x, eye2_center_y

    file_name= "smiley_face"
    ! Initialize the canvas
    call canva%init()
    call canva%set_background(FIG_COLOR_WHITE)

    ! Set parameters
    face_center_x = CANVAS_WIDTH / 2
    face_center_y = CANVAS_HEIGHT / 2
    eye1_center_x = face_center_x - FACE_RADIUS / 2
    eye1_center_y = face_center_y - FACE_RADIUS / 3
    eye2_center_x = face_center_x + FACE_RADIUS / 2
    eye2_center_y = face_center_y - FACE_RADIUS / 3

    ! Draw face
    circ%center%x = face_center_x / CANVAS_WIDTH
    circ%center%y = face_center_y / CANVAS_HEIGHT
    circ%r = FACE_RADIUS
    circ%stroke_color = FIG_COLOR_BLACK
    circ%fill_color = FIG_COLOR_YELLOW
    call canva%add_shape(circ)

    ! Draw left eye
    circ%center%x = eye1_center_x / CANVAS_WIDTH
    circ%center%y = eye1_center_y / CANVAS_HEIGHT
    circ%r = EYE_RADIUS
    circ%stroke_color = FIG_COLOR_WHITE
    circ%fill_color = FIG_COLOR_WHITE
    call canva%add_shape(circ)
    circ%r = EYE_RADIUS / 2
    circ%fill_color = FIG_COLOR_BLACK
    call canva%add_shape(circ)

    ! Draw right eye
    circ%center%x = eye2_center_x / CANVAS_WIDTH
    circ%center%y = eye2_center_y / CANVAS_HEIGHT
    circ%r = EYE_RADIUS
    circ%stroke_color = FIG_COLOR_WHITE
    circ%fill_color = FIG_COLOR_WHITE
    call canva%add_shape(circ)
    circ%r = EYE_RADIUS / 2
    circ%fill_color = FIG_COLOR_BLACK
    call canva%add_shape(circ)

    ! Draw mouth
    ellip%center%x = face_center_x / CANVAS_WIDTH
    ellip%center%y = (face_center_y + FACE_RADIUS / 3) / CANVAS_HEIGHT
    ellip%rx = MOUTH_RADIUS_X
    ellip%ry = MOUTH_RADIUS_Y
    ellip%fill_color = FIG_COLOR_RED
    ellip%stroke_color = FIG_COLOR_RED
    call canva%add_shape(ellip)

    ! Save to bitmap and SVG
    call bitmap_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT)
    call bitmap_canva%save_to_file(canva, file_name)

    call svg_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT)
    call svg_canva%save_to_file(canva, file_name)

    call bitmap_canva%destroy()
    call test_both(file_name,bitmap_canva)

end program smile

