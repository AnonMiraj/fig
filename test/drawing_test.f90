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
    type(triangle) :: tri
    type(arc) :: ar
    type(text) :: te
    type(path) :: ph,path2
    type(polyline) :: pl
    type(polygon) :: pg
    real :: x_points(7) = [0.0, 40.0, 40.0, 80.0, 80.0, 120.0, 120.0]
    real :: y_points(7) = [120.0, 120.0, 160.0, 160.0, 200.0, 200.0, 240.0]
    real :: polygon_x_points(5) = [700.0, 640.0, 790.0, 610.0, 760.0]
    real :: polygon_y_points(5) = [10.0, 198.0, 78.0, 78.0, 198.0]
    integer :: n=7,n2=5

    type(RGB) :: bg, color
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva
    real(8) :: pi = 4.0d0 * atan(1.0d0)

    x_points=x_points/WIDTH
    y_points=y_points/HEIGHT

    polygon_x_points=polygon_x_points/2.
    polygon_y_points=polygon_y_points/2.

    polygon_x_points=polygon_x_points/WIDTH
    polygon_y_points=polygon_y_points/HEIGHT

    file_name = "test_all"
    bg = FIG_COLOR_WHITE

    call canva%init()
    canva%background=FIG_COLOR_WHITE

   ! Circle
    c%c%x = 100.0 / WIDTH
    c%c%y = 100.0 / HEIGHT
    c%r = 50.0
    c%fill_color = FIG_COLOR_PINK
    c%fill_color%a = .5
    c%stroke_color = FIG_COLOR_RED
    c%stroke_color%a = .5
    call canva%add_shape(c)

    ! Ellipse
    elp%c%x = 250.0 / WIDTH
    elp%c%y = 100.0 / HEIGHT
    elp%rx = 50.0 
    elp%ry = 25.0 
    color = FIG_COLOR_GOLD
    color%a = .5
    elp%fill_color = color
    elp%stroke_color = FIG_COLOR_RED
    elp%stroke_color%a = .5
    call canva%add_shape(elp)

    ! Rectangle 1
    r%p%x = 200.0 / WIDTH
    r%p%y = 200.0 / HEIGHT
    r%width = 100.0 
    r%height = 50.0 
    r%fill_color = FIG_COLOR_BLUE
    r%fill_color%a = .5
    r%stroke_width = 10
    r%stroke_color = FIG_COLOR_GOLD
    call canva%add_shape(r)

    ! Rectangle 2
    r%p%x = 250.0 / WIDTH
    r%p%y = 220.0 / HEIGHT
    r%width = 120.0 
    r%height = 50.0 
    r%fill_color = FIG_COLOR_RED
    r%fill_color%a = .5
    r%rx=5
    r%ry=5
    r%stroke_width = 6
    r%stroke_color = FIG_COLOR_SEAGREEN
    r%stroke_color%a=.5
    call canva%add_shape(r)

    ! Line 1
    l%p1%x = 400.0 / WIDTH
    l%p1%y = 200.0 / HEIGHT
    l%p2%x = 550.0 / WIDTH
    l%p2%y = 450.0 / HEIGHT
    l%stroke_color = FIG_COLOR_BLACK
    l%stroke_width = 10
    call canva%add_shape(l)
    l%stroke_width = 2
    l%stroke_color = FIG_COLOR_RED
    l%stroke_color%a=.5
    call canva%add_shape(l)


    ! Line
    l%p1%x = 400.0 / WIDTH
    l%p1%y = 400.0 / HEIGHT
    l%p2%x = 550.0 / WIDTH
    l%p2%y = 400.0 / HEIGHT
    l%stroke_color = FIG_COLOR_BLACK
    l%stroke_color%a=.5
    l%stroke_width = 50
    call canva%add_shape(l)
    l%stroke_width = 2
    l%stroke_color = FIG_COLOR_RED
    l%stroke_color%a=.5

    call canva%add_shape(l)


    ! Arc
    ar%c%x = 300.0 / WIDTH
    ar%c%y = 400.0 / HEIGHT
    ar%r = 50.0
    ar%start_angle= 0
    ar%end_angle= pi * 1.2
    ar%fill_color = FIG_COLOR_INDIGO
    ar%fill_color%a=.5
    ar%stroke_color = FIG_COLOR_BROWN
    call canva%add_shape(ar)
   
    
    ! Text
    te%p%x=.0
    te%p%y=.5
    te%slant=FIG_FONT_SLANT_ITALIC
    te%weight=FIG_FONT_WEIGHT_BOLD
    te%size = 20
    te%content="Hello, Fortran"
    te%font_family="Fira Sans"
    te%fill_color=FIG_COLOR_RED
    te%stroke_color=FIG_COLOR_BLACK
    call canva%add_shape(te)

    ! Triangle
    tri%p1%x = 450.0 / WIDTH
    tri%p1%y = 150.0 / HEIGHT
    tri%p2%x = 550.0 / WIDTH
    tri%p2%y = 250.0 / HEIGHT
    tri%p3%x = 450.0 / WIDTH
    tri%p3%y = 350.0 / HEIGHT
    tri%fill_color = FIG_COLOR_SILVER
    tri%fill_color%a = 100
    tri%stroke_color = FIG_COLOR_RED
    call canva%add_shape(tri)

    ! Text
    te%p%x=.0
    te%p%y=.7
    te%slant=FIG_FONT_SLANT_OBLIQUE
    te%weight=FIG_FONT_WEIGHT_NORMAL
    te%size = 20
    te%content="Hello, Fortran"
    te%font_family="Fira Sans"
    te%fill_color=FIG_COLOR_RED
    te%stroke_color=FIG_COLOR_BLACK
    call canva%add_shape(te)


    ! Path 
    call ph%move_to(0.1, 0.9)
    call ph%line_to(0.3, 0.85)
    call ph%bezier_curve_to(0.35, 0.8, 0.45, 0.7, 0.5, 0.85)
    call ph%quadratic_curve_to(0.7, 0.95, 0.9, 0.85)
    call ph%elliptical_arc_to(0.8, 0.9, 0.15, 0, 1, 1.0, 1.0)
    call ph%close_path()
    ph%fill_color = FIG_COLOR_LIGHTPINK
    ph%stroke_color = FIG_COLOR_DARKRED
    allocate(ph%dash_array(2))
    ph%dash_array(1) = 4.
    ph%dash_array(2) = 4.
    call canva%add_shape(ph)

    ! Path 2
    call path2%move_to(0.15, 0.85)
    call path2%line_to(0.35, 0.88)
    call path2%bezier_curve_to(0.5, 0.82, 0.65, 0.87, 0.75, 0.9)
    call path2%quadratic_curve_to(0.85, 0.97, 1.0, 0.9)
    call path2%elliptical_arc_to(0.6, 0.95, 0.1, 0, 1, 0.85, 0.95)
    call path2%close_path()

    path2%fill_color = FIG_COLOR_LIGHTGREEN
    path2%fill_color%a=0.5
    path2%stroke_color = FIG_COLOR_DARKGREEN

    allocate(path2%dash_array(2))
    path2%dash_array(1) = 5.
    path2%dash_array(2) = 5.
    call canva%add_shape(path2)

    ! polyline
    call pl%add_points(x_points, y_points, n)
    pl%stroke_color=FIG_COLOR_RED
    pl%fill_color=FIG_COLOR_YELLOW
    pl%stroke_width=4
    call canva%add_shape(pl)

    ! polygon
    call pg%add_points(polygon_x_points, polygon_y_points, n2)
    pg%stroke_color=FIG_COLOR_BLACK
    pg%fill_color=FIG_COLOR_YELLOW
    pg%stroke_width=4
    call canva%add_shape(pg)



    call draw_to_png(canva , WIDTH, HEIGHT, file_name)
    call draw_to_svg(canva , WIDTH, HEIGHT, file_name)

    call test_both(file_name)
end program drawing_test_all
