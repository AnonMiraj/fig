program smile
   use fig
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

   file_name = "smiley_face"
   call canva%init()
   canva%background = FIG_COLOR_WHITE

   face_center_x = CANVAS_WIDTH/2
   face_center_y = CANVAS_HEIGHT/2
   eye1_center_x = face_center_x - FACE_RADIUS/2
   eye1_center_y = face_center_y - FACE_RADIUS/3
   eye2_center_x = face_center_x + FACE_RADIUS/2
   eye2_center_y = face_center_y - FACE_RADIUS/3

   ! Draw face
   circ%c%x = face_center_x/CANVAS_WIDTH
   circ%c%y = face_center_y/CANVAS_HEIGHT
   circ%r = FACE_RADIUS
   circ%stroke_color = FIG_COLOR_BLACK
   circ%fill_color = FIG_COLOR_YELLOW
   call canva%add_shape(circ)

   ! Draw left eye
   circ%c%x = eye1_center_x/CANVAS_WIDTH
   circ%c%y = eye1_center_y/CANVAS_HEIGHT
   circ%r = EYE_RADIUS
   circ%stroke_color = FIG_COLOR_WHITE
   circ%fill_color = FIG_COLOR_WHITE
   call canva%add_shape(circ)
   circ%r = EYE_RADIUS/2
   circ%fill_color = FIG_COLOR_BLACK
   call canva%add_shape(circ)

   ! Draw right eye
   circ%c%x = eye2_center_x/CANVAS_WIDTH
   circ%c%y = eye2_center_y/CANVAS_HEIGHT
   circ%r = EYE_RADIUS
   circ%stroke_color = FIG_COLOR_WHITE
   circ%fill_color = FIG_COLOR_WHITE
   call canva%add_shape(circ)
   circ%r = EYE_RADIUS/2
   circ%fill_color = FIG_COLOR_BLACK
   call canva%add_shape(circ)

   ! Draw mouth
   ellip%c%x = face_center_x/CANVAS_WIDTH
   ellip%c%y = (face_center_y + FACE_RADIUS/3)/CANVAS_HEIGHT
   ellip%rx = MOUTH_RADIUS_X
   ellip%ry = MOUTH_RADIUS_Y
   ellip%fill_color = FIG_COLOR_RED
   ellip%stroke_color = FIG_COLOR_RED
   call canva%add_shape(ellip)

   ! Save to bitmap and SVG
   call svg_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT, file_name)
   call svg_canva%apply_shapes(canva)
   call svg_canva%save_to_svg()
   call svg_canva%destroy()
   call bitmap_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT, file_name)
   call bitmap_canva%apply_shapes(canva)
   call bitmap_canva%save_to_png()
   call bitmap_canva%save_to_ppm()
   call bitmap_canva%destroy()

   call test_both(file_name)

end program smile

