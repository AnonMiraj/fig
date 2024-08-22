program test_fig_fill_triangle
   use fig
   use fig_test
   implicit none

   integer, parameter :: WIDTH = 801
   integer, parameter :: HEIGHT = 801
   integer, parameter :: TRIANGLE_SIZE = 50
   character(len=:), allocatable  :: file_name

   type(drawing) :: canva
   type(triangle) :: tri
   type(circle) :: circ
   type(RGB), dimension(0:6) :: colors

   integer :: i, j, ind, ind1, ind2

   file_name = "cool_triangle_pattern"
   call canva%init()
   canva%background = FIG_COLOR_WHITE

   colors = [FIG_COLOR_RED, &
             FIG_COLOR_MAGENTA, &
             FIG_COLOR_YELLOW, &
             FIG_COLOR_GREEN, &
             FIG_COLOR_CYAN, &
             FIG_COLOR_PINK, &
             FIG_COLOR_TAN]

   tri%stroke_color = FIG_COLOR_BLACK
   circ%stroke_color = FIG_COLOR_BLACK
   circ%r = 5.0

   do i = 0, WIDTH - 1, TRIANGLE_SIZE
      do j = 0, HEIGHT - 1, TRIANGLE_SIZE
         ind = mod(i + 2*j, 7)
         ind1 = mod(ind*2, 7)
         ind2 = mod(ind*3, 7)

         tri%p1%x = real(i)/WIDTH
         tri%p1%y = real(j)/HEIGHT
         tri%p2%x = real(i + TRIANGLE_SIZE)/WIDTH
         tri%p2%y = real(j)/HEIGHT
         tri%p3%x = real(i + TRIANGLE_SIZE)/WIDTH
         tri%p3%y = real(j + TRIANGLE_SIZE)/HEIGHT
         tri%fill_color = colors(ind1)
         call canva%add_shape(tri)

         tri%p2%x = real(i)/WIDTH
         tri%p2%y = real(j + TRIANGLE_SIZE)/HEIGHT
         tri%fill_color = colors(ind2)
         call canva%add_shape(tri)

         circ%c%x = real(i)/WIDTH
         circ%c%y = real(j)/HEIGHT
         circ%fill_color = colors(ind1)
         call canva%add_shape(circ)

         circ%c%x = real(i + TRIANGLE_SIZE)/WIDTH
         call canva%add_shape(circ)

         circ%c%y = real(j + TRIANGLE_SIZE)/HEIGHT
         call canva%add_shape(circ)

         circ%c%x = real(i)/WIDTH
         call canva%add_shape(circ)

         circ%c%x = real(i + TRIANGLE_SIZE/2)/WIDTH
         circ%c%y = real(j + TRIANGLE_SIZE/2)/HEIGHT
         call canva%add_shape(circ)
      end do
   end do

   call draw_to_png(canva, WIDTH, HEIGHT, file_name)
   call draw_to_svg(canva, WIDTH, HEIGHT, file_name)

   call test_both(file_name)
contains
end program test_fig_fill_triangle

