program gradient_test
    use fig_canvas
    use fig_drawing
    use fig_rgb
    use fig_shapes
    use fig_rgb_color_constants
    use fig_bitmap
    use fig_test
    implicit none
    integer, parameter :: WIDTH = 800
    integer, parameter :: HEIGHT = 800
    integer, parameter :: cols = 7
    integer, parameter :: rows = 4
    real(kind=8), parameter :: CELL_WIDTH = (WIDTH/real(cols))
    real(kind=8), parameter :: CELL_HEIGHT =(HEIGHT/real(rows))
    integer :: x, y
    character(len=:), allocatable  :: file_name

    type(drawing) :: checker
    type(bitmap_canvas) :: bitmap_canva
    type(rectangle) :: rect
    type(RGB) :: ALTERNATE_COLOR, BACKGROUND_COLOR, color
    type(linear_gradient) :: gg
    file_name = "checker_grad"
    call checker%init()

    BACKGROUND_COLOR = FIG_COLOR_BLACK
    ALTERNATE_COLOR = FIG_COLOR_WHITE
    
    color=FIG_COLOR_RED
    call gg%stops%init()

    do y = 0, rows - 1
        do x = 0, cols - 1

                gg%x1=x*CELL_WIDTH
                gg%y1=y*CELL_HEIGHT
                gg%x2=x*CELL_WIDTH+CELL_WIDTH
                gg%y2=y*CELL_HEIGHT+CELL_HEIGHT
        select case (mod(x  + y*cols, 3) )
            case (0)

                call gg%stops%add_stop(0.0_c_double,FIG_COLOR_RED)
                call gg%stops%add_stop(1.0_c_double,FIG_COLOR_AQUA)
                rect%fill_color2%pat=color
                 ! rect%fill_color2%pat=gg
                rect%stroke_color = FIG_COLOR_AQUA
            case (1)
                call gg%stops%add_stop(0.0_c_double,FIG_COLOR_BLUE)
                call gg%stops%add_stop(1.0_c_double,FIG_COLOR_GOLD)
                 ! rect%fill_color=FIG_COLOR_BLUE
                rect%fill_color2%pat=gg
                rect%stroke_color = FIG_COLOR_BEIGE
            case (2)
                call gg%stops%add_stop(0.0_c_double,FIG_COLOR_LIME)
                call gg%stops%add_stop(1.0_c_double,FIG_COLOR_DARKKHAKI)
                rect%fill_color2%pat=gg
                  ! rect%fill_color=FIG_COLOR_LIME
                rect%stroke_color = FIG_COLOR_BLACK
        !     case (3)
        !         call gg%stops%add_stop(0.0_c_double,FIG_COLOR_YELLOW)
        !         call gg%stops%add_stop(1.0_c_double,FIG_COLOR_WHITE)
                ! rect%fill_color=FIG_COLOR_WHITE
        !         rect%fill_color=gg
        end select
            call gg%stops%clear_stops
            rect%upper_left%x = (x * 1.0 / cols)
            rect%upper_left%y = (y * 1.0 / rows)
            rect%width = CELL_WIDTH
            rect%height = CELL_HEIGHT
            ! rect%fill_color = color
            rect%stroke_width = 3

            call checker%add_shape(rect)
        end do
    end do
    call bitmap_canva%init(HEIGHT,WIDTH)
    call bitmap_canva%save_to_file(checker,file_name,"png")
    call bitmap_canva%save_to_file(checker,file_name,"ppm")
    call bitmap_canva%destroy()

    call test_both(file_name,bitmap_canva)

end program gradient_test

