program radial_lines
   use fig
   use fig_test
   implicit none

   integer, parameter :: WIDTH = 800
   integer, parameter :: HEIGHT = 800
   integer, parameter :: NUM_LINES = 180
   character(len=:), allocatable  :: file_name

   type(drawing) :: canva
   type(line) :: sh
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

   call canva%init()
   canva%background = FIG_COLOR_BLACK

   cx = WIDTH/2
   cy = HEIGHT/2
   radius = 300
   angle_step = 1.0*atan(1.0)/NUM_LINES

   do i = 0, NUM_LINES - 1
      angle = 4*i*angle_step
      call draw_radial_line(canva, cx, cy, radius, angle)
   end do

   call draw_to_png(canva, WIDTH, HEIGHT, file_name)
   call draw_to_svg(canva, WIDTH, HEIGHT, file_name)

   call test_both(file_name)
contains

   subroutine draw_radial_line(canva, cx, cy, radius, angle)
      type(drawing), intent(inout) :: canva
      real, intent(in) :: cx, cy, radius
      real, intent(in) :: angle
      real :: cos_angle, sin_angle

      cos_angle = cos(angle)
      sin_angle = sin(angle)
      sh%p1%x = (cx + int(radius*cos_angle))/WIDTH
      sh%p1%y = (cy + int(radius*sin_angle))/HEIGHT
      sh%p2%x = (cx - int(radius*cos_angle))/WIDTH
      sh%p2%y = (cy - int(radius*sin_angle))/HEIGHT
      sh%stroke_color = color_palette(mod(ind, 8) + 1)
      ind = ind + 1

      call canva%add_shape(sh)
   end subroutine draw_radial_line
end program radial_lines

