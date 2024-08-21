program drawing_test_all
   use fig
   use fig_test
   implicit none

   integer, parameter :: WIDTH = 600.0
   integer, parameter :: HEIGHT = 600.0
   character(len=:), allocatable  :: file_name
   type(drawing) :: canva
   type(circle) :: c
   type(rectangle) :: r
   type(line) :: l
   type(ellipse) :: elp
   type(arc) :: ar
   type(text) :: te
   type(RGB) :: bg, color
   real(8) :: pi = 4.0d0*atan(1.0d0)
   file_name = "test_all"
   bg = FIG_COLOR_WHITE

   call canva%init()
   canva%background = FIG_COLOR_WHITE

   ! Circle
   c%c%x = 100.0/WIDTH
   c%c%y = 100.0/HEIGHT
   c%r = 50.0
   c%fill_color = FIG_COLOR_PINK
   c%fill_color%a = .5
   c%stroke_color = FIG_COLOR_RED
   c%stroke_color%a = .5
   call canva%add_shape(c)

   ! Ellipse
   elp%c%x = 250.0/WIDTH
   elp%c%y = 100.0/HEIGHT
   elp%rx = 50.0
   elp%ry = 25.0
   color = FIG_COLOR_GOLD
   color%a = .5
   elp%fill_color = color
   elp%stroke_color = FIG_COLOR_RED
   elp%stroke_color%a = .5
   call canva%add_shape(elp)

   ! Rectangle 1
   r%p%x = 200.0/WIDTH
   r%p%y = 200.0/HEIGHT
   r%width = 100.0
   r%height = 50.0
   r%fill_color = FIG_COLOR_BLUE
   r%fill_color%a = .5
   r%stroke_width = 10
   r%stroke_color = FIG_COLOR_GOLD
   call canva%add_shape(r)

   ! Rectangle 2
   r%p%x = 250.0/WIDTH
   r%p%y = 220.0/HEIGHT
   r%width = 120.0
   r%height = 50.0
   r%fill_color = FIG_COLOR_RED
   r%fill_color%a = .5
   r%rx = 5
   r%ry = 5
   r%stroke_width = 6
   r%stroke_color = FIG_COLOR_SEAGREEN
   r%stroke_color%a = .5
   call canva%add_shape(r)

   ! Line 1
   l%p1%x = 400.0/WIDTH
   l%p1%y = 200.0/HEIGHT
   l%p2%x = 550.0/WIDTH
   l%p2%y = 450.0/HEIGHT
   l%stroke_color = FIG_COLOR_BLACK
   l%stroke_width = 10
   call canva%add_shape(l)
   l%stroke_width = 2
   l%stroke_color = FIG_COLOR_RED
   l%stroke_color%a = .5
   call canva%add_shape(l)

   ! Line
   l%p1%x = 400.0/WIDTH
   l%p1%y = 400.0/HEIGHT
   l%p2%x = 550.0/WIDTH
   l%p2%y = 400.0/HEIGHT
   l%stroke_color = FIG_COLOR_BLACK
   l%stroke_color%a = .5
   l%stroke_width = 50
   call canva%add_shape(l)
   l%stroke_width = 2
   l%stroke_color = FIG_COLOR_RED
   l%stroke_color%a = .5

   call canva%add_shape(l)

   ! Arc
   ar%c%x = 300.0/WIDTH
   ar%c%y = 400.0/HEIGHT
   ar%r = 50.0
   ar%start_angle = 0
   ar%end_angle = pi*1.2
   ar%fill_color = FIG_COLOR_INDIGO
   ar%fill_color%a = .5
   ar%stroke_color = FIG_COLOR_BROWN
   call canva%add_shape(ar)

   ! Text
   te%p%x = .0
   te%p%y = .5
   te%slant = FIG_FONT_SLANT_ITALIC
   te%weight = FIG_FONT_WEIGHT_BOLD
   te%size = 20
   te%content = "Hello, Fortran"
   te%font_family = "Fira Sans"
   te%fill_color = FIG_COLOR_RED
   te%stroke_color = FIG_COLOR_BLACK
   call canva%add_shape(te)

   ! Text
   te%p%x = .0
   te%p%y = .7
   te%slant = FIG_FONT_SLANT_OBLIQUE
   te%weight = FIG_FONT_WEIGHT_NORMAL
   te%size = 20
   te%content = "Hello, Fortarn"
   te%font_family = "Fira Sans"
   te%fill_color = FIG_COLOR_RED
   te%stroke_color = FIG_COLOR_BLACK
   call canva%add_shape(te)

   call draw_to_png(canva, WIDTH, HEIGHT, file_name)
   call draw_to_svg(canva, WIDTH, HEIGHT, file_name)

   call test_both(file_name)
end program drawing_test_all

