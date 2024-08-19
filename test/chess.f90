program chess_checker
    use fig
    use fig_test
    implicit none
    integer, parameter :: WIDTH = 800
    integer, parameter :: HEIGHT = 800
    integer, parameter :: cols = 8
    integer, parameter :: rows = 8
    integer, parameter :: CELL_WIDTH = (WIDTH/cols)
    integer, parameter :: CELL_HEIGHT =(HEIGHT/rows)
    integer :: x, y
    character(len=:), allocatable  :: file_name

    type(drawing) :: checker
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva
    type(rectangle) :: rect
    type(RGB) :: ALTERNATE_COLOR, BACKGROUND_COLOR
    file_name = "checker"
    call checker%init()

    BACKGROUND_COLOR = FIG_COLOR_BLACK
    ALTERNATE_COLOR = FIG_COLOR_WHITE


    do y = 0, rows - 1
        do x = 0, cols - 1

            rect%p%x = (x * 1.0 / cols)
            rect%p%y = (y * 1.0 / rows)
            rect%width = CELL_WIDTH
            rect%height = CELL_HEIGHT

            if (mod(x + y, 2) == 0) then
                rect%fill_color = ALTERNATE_COLOR
            else
                rect%fill_color = BACKGROUND_COLOR
            endif

            call checker%add_shape(rect)
        end do
    end do
    call svg_canva%init(WIDTH,HEIGHT,file_name)
    call svg_canva%apply_shapes(checker)
    call svg_canva%save_to_svg()
    call svg_canva%destroy()
    call bitmap_canva%init(HEIGHT,WIDTH,file_name)
    call bitmap_canva%apply_shapes(checker)
    call bitmap_canva%save_to_png()
    call bitmap_canva%save_to_ppm()
    call bitmap_canva%destroy()

    call test_both(file_name,bitmap_canva)

end program chess_checker
