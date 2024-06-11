program chess_checker
    use fig_canvas
    use fig_drawing
    use fig_rgb
    use fig_shapes
    use fig_rgb_color_constants
    use fig_svg
    use fig_bitmap
    implicit none
    integer, parameter :: WIDTH = 800
    integer, parameter :: HEIGHT = 800
    integer, parameter :: cols = 8
    integer, parameter :: rows = 8
    integer, parameter :: CELL_WIDTH = (WIDTH/cols)
    integer, parameter :: CELL_HEIGHT =(HEIGHT/rows)
    integer :: x, y

    type(drawing) :: checker
    type(svg_canvas) :: svg_canva
    type(bitmap_canvas) :: bitmap_canva
    type(rectangle) :: rect
    type(RGB) :: ALTERNATE_COLOR, BACKGROUND_COLOR, color

    call checker%init()

    BACKGROUND_COLOR = FIG_COLOR_BLACK
    ALTERNATE_COLOR = FIG_COLOR_WHITE


    do y = 0, rows - 1
        do x = 0, cols - 1
            if (mod(x + y, 2) == 0) then
                color = ALTERNATE_COLOR
            else
                color = BACKGROUND_COLOR
            endif

            rect%upper_left%x = (x * 1.0 / cols)
            rect%upper_left%y = (y * 1.0 / rows)
            rect%width = CELL_WIDTH
            rect%height = CELL_HEIGHT
            rect%fill_color = color

            call checker%add_shape(rect)
        end do
    end do
    call svg_canva%init(HEIGHT,WIDTH)
    call svg_canva%save_to_file(checker,"checker")
    call bitmap_canva%init(HEIGHT,WIDTH)
    call bitmap_canva%save_to_file(checker,"checker")


    print *, 'Image successfully saved to checker.(ppm\svg)'

end program chess_checker
