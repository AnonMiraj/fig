program circles_pattern
   use fig
   use fig_test
   implicit none
   integer, parameter :: WIDTH = 800
   integer, parameter :: HEIGHT = 600
   integer, parameter :: cols = 8*2
   integer, parameter :: rows = 6*2
   integer, parameter :: CELL_WIDTH = (WIDTH/cols)
   integer, parameter :: CELL_HEIGHT = (HEIGHT/rows)
   character(len=:), allocatable  :: file_name
   integer :: x, y
   real :: center_x, center_y
   real :: t
   real :: radius
   type(drawing) :: canva
   type(RGB), dimension(8) :: color_palette
   type(circle) :: circ

   file_name = "circles_pattern"
   call canva%init()

   color_palette = [FIG_COLOR_RED, &
                    FIG_COLOR_MAGENTA, &
                    FIG_COLOR_YELLOW, &
                    FIG_COLOR_GREEN, &
                    FIG_COLOR_BLUE, &
                    FIG_COLOR_CYAN, &
                    FIG_COLOR_PINK, &
                    FIG_COLOR_WHITE]

   do y = 0, rows - 1
      do x = 0, cols - 1
         center_x = (x + 0.5)/real(cols)
         center_y = (y + 0.5)/real(rows)

         t = (real(x) + real(y))/real(cols + rows - 2)
         radius = min(CELL_WIDTH, CELL_HEIGHT)*lerpf(0.125, 0.5, t)

         circ%r = radius
         circ%c%x = center_x
         circ%c%y = center_y
         circ%fill_color = color_palette(mod(3*x + 2*y, 8) + 1)
         circ%stroke_color = color_palette(mod(2*x + 3*y, 8) + 1)
         call canva%add_shape(circ)
      end do
   end do

   call draw_to_png(canva, WIDTH, HEIGHT, file_name)
   call draw_to_svg(canva, WIDTH, HEIGHT, file_name)

   call test_both(file_name)
contains

   function lerpf(a, b, t)
      real :: lerpf
      real, intent(in) :: a, b, t
      lerpf = a + (b - a)*t
   end function lerpf
end program circles_pattern
