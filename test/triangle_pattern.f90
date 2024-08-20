program test_fig_fill_triangle
   use fig
   use fig_test
   implicit none

   integer, parameter :: CANVAS_WIDTH = 801
   integer, parameter :: CANVAS_HEIGHT = 801
   integer, parameter :: TRIANGLE_SIZE = 50
   character(len=:), allocatable  :: file_name

   type(drawing) :: test_canvas
   type(triangle) :: tri
   type(circle) :: circ
   type(RGB), dimension(0:6) :: colors
   type(svg_canvas) :: svg_canva
   type(bitmap_canvas) :: bitmap_canva

   integer :: i, j, ind, ind1, ind2

   file_name = "cool_triangle_pattern"
   call test_canvas%init()
   test_canvas%background = FIG_COLOR_WHITE

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

   do i = 0, CANVAS_WIDTH - 1, TRIANGLE_SIZE
      do j = 0, CANVAS_HEIGHT - 1, TRIANGLE_SIZE
         ind = mod(i + 2*j, 7)
         ind1 = mod(ind*2, 7)
         ind2 = mod(ind*3, 7)

         tri%p1%x = real(i)/CANVAS_WIDTH
         tri%p1%y = real(j)/CANVAS_HEIGHT
         tri%p2%x = real(i + TRIANGLE_SIZE)/CANVAS_WIDTH
         tri%p2%y = real(j)/CANVAS_HEIGHT
         tri%p3%x = real(i + TRIANGLE_SIZE)/CANVAS_WIDTH
         tri%p3%y = real(j + TRIANGLE_SIZE)/CANVAS_HEIGHT
         tri%fill_color = colors(ind1)
         call test_canvas%add_shape(tri)

         tri%p2%x = real(i)/CANVAS_WIDTH
         tri%p2%y = real(j + TRIANGLE_SIZE)/CANVAS_HEIGHT
         tri%fill_color = colors(ind2)
         call test_canvas%add_shape(tri)

         circ%c%x = real(i)/CANVAS_WIDTH
         circ%c%y = real(j)/CANVAS_HEIGHT
         circ%fill_color = colors(ind1)
         call test_canvas%add_shape(circ)

         circ%c%x = real(i + TRIANGLE_SIZE)/CANVAS_WIDTH
         call test_canvas%add_shape(circ)

         circ%c%y = real(j + TRIANGLE_SIZE)/CANVAS_HEIGHT
         call test_canvas%add_shape(circ)

         circ%c%x = real(i)/CANVAS_WIDTH
         call test_canvas%add_shape(circ)

         circ%c%x = real(i + TRIANGLE_SIZE/2)/CANVAS_WIDTH
         circ%c%y = real(j + TRIANGLE_SIZE/2)/CANVAS_HEIGHT
         call test_canvas%add_shape(circ)
      end do
   end do

   call svg_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT, file_name)
   call svg_canva%apply_shapes(test_canvas)
   call svg_canva%save_to_svg()
   call svg_canva%destroy()
   call bitmap_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT, file_name)
   call bitmap_canva%apply_shapes(test_canvas)
   call bitmap_canva%save_to_png()
   call bitmap_canva%save_to_ppm()
   call bitmap_canva%destroy()

   call test_both(file_name)
contains
end program test_fig_fill_triangle

