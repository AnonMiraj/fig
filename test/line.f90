program radial_lines
   use fig
   use fig_test
   implicit none

   integer, parameter :: CANVAS_WIDTH = 800
   integer, parameter :: CANVAS_HEIGHT = 800
   integer, parameter :: NUM_LINES = 180
   character(len=:), allocatable  :: file_name

   type(drawing) :: radial_canvas
   type(line) :: sh
   type(svg_canvas) :: svg_canva
   type(bitmap_canvas) :: bitmap_canva
   type(RGB), dimension(8) :: color_palette
   integer :: i, ind = 0
   real :: cx, cy, radius
   real :: angle_step, angle
   file_name = "radial_lines"

   color_palette = [FIG_COLOR_RED, &
                    FIG_COLOR_MAGENTA, &
                    FIG_COLOR_YELLOW, &
                    FIG_COLOR_GREEN, &
                    FIG_COLOR_BLUE, &
                    FIG_COLOR_CYAN, &
                    FIG_COLOR_PINK, &
                    FIG_COLOR_WHITE]

   call radial_canvas%init()
   radial_canvas%background = FIG_COLOR_BLACK

   cx = CANVAS_WIDTH/2
   cy = CANVAS_HEIGHT/2
   radius = 300
   angle_step = 1.0*atan(1.0)/NUM_LINES

   do i = 0, NUM_LINES - 1
      angle = 4*i*angle_step
      call draw_radial_line(radial_canvas, cx, cy, radius, angle)
   end do

   call svg_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT, file_name)
   call svg_canva%apply_shapes(radial_canvas)
   call svg_canva%save_to_svg()
   call svg_canva%destroy()
   call bitmap_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT, file_name)
   call bitmap_canva%apply_shapes(radial_canvas)
   call bitmap_canva%save_to_png()
   call bitmap_canva%save_to_ppm()
   call bitmap_canva%destroy()

   call test_both(file_name)
contains

   subroutine draw_radial_line(canva, cx, cy, radius, angle)
      type(drawing), intent(inout) :: canva
      real, intent(in) :: cx, cy, radius
      real, intent(in) :: angle
      real :: cos_angle, sin_angle

      cos_angle = cos(angle)
      sin_angle = sin(angle)
      sh%p1%x = (cx + int(radius*cos_angle))/CANVAS_WIDTH
      sh%p1%y = (cy + int(radius*sin_angle))/CANVAS_HEIGHT
      sh%p2%x = (cx - int(radius*cos_angle))/CANVAS_WIDTH
      sh%p2%y = (cy - int(radius*sin_angle))/CANVAS_HEIGHT
      sh%stroke_color = color_palette(mod(ind, 8) + 1)
      ind = ind + 1

      call canva%add_shape(sh)
   end subroutine draw_radial_line
end program radial_lines

