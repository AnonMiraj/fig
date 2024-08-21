program test_fig_draw_triangle
   use fig
   use fig_test
   implicit none

   integer, parameter :: WIDTH = 801
   integer, parameter :: HEIGHT = 801
   integer, parameter :: TRIANGLE_SIZE = 50
   character(len=:), allocatable  :: file_name

   type(drawing) :: canva
   type(triangle) :: tri
   type(RGB), dimension(0:6) :: colors

   integer :: i, j, ind

   file_name = "triangles"
   call canva%init()
   canva%background = FIG_COLOR_WHITE

   colors(0) = FIG_COLOR_RED
   colors(1) = FIG_COLOR_PINK
   colors(2) = FIG_COLOR_YELLOW
   colors(3) = FIG_COLOR_GREEN
   colors(4) = FIG_COLOR_BLUE
   colors(5) = FIG_COLOR_CYAN
   colors(6) = FIG_COLOR_MAGENTA

   tri%fill_color = FIG_COLOR_BLACK

   do i = 0, WIDTH - 1, TRIANGLE_SIZE
      do j = 0, HEIGHT - 1, TRIANGLE_SIZE
         ind = mod(i + j, 7)
         tri%p1%x = real(i)/WIDTH
         tri%p1%y = real(j)/HEIGHT
         tri%p2%x = real(i + TRIANGLE_SIZE)/WIDTH
         tri%p2%y = real(j)/HEIGHT
         tri%p3%x = real(i + TRIANGLE_SIZE)/WIDTH
         tri%p3%y = real(j + TRIANGLE_SIZE)/HEIGHT
         tri%stroke_color = colors(ind)

         call canva%add_shape(tri)

         tri%p2%x = real(i)/WIDTH
         tri%p2%y = real(j + TRIANGLE_SIZE)/HEIGHT
         call canva%add_shape(tri)
      end do
   end do

   call draw_to_png(canva, WIDTH, HEIGHT, file_name)
   call draw_to_svg(canva, WIDTH, HEIGHT, file_name)

   call test_both(file_name)
end program test_fig_draw_triangle

