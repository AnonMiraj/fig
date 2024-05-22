program chess_checker
    use fig_canvas
    use fig_g_canvas
    use fig_rgb
    use fig_shapes
    implicit none
    integer, parameter :: WIDTH = 800
    integer, parameter :: HEIGHT = 800
    integer, parameter :: cols = 8
    integer, parameter :: rows = 8
    integer, parameter :: CELL_WIDTH = (WIDTH/cols)
    integer, parameter :: CELL_HEIGHT =(HEIGHT/rows)
    integer :: x,y

    type(g_canvas) :: checker
    type(rectangle) :: rect
    type(RGB) ::ALTERNATE_COLOR,BACKGROUND_COLOR, color
    call checker%init(real(WIDTH), real(HEIGHT), 'checker')
    
    BACKGROUND_COLOR = BLACK 
    ALTERNATE_COLOR = WHITE 
    rect%x=0
    rect%y=0
    rect%width=WIDTH
    rect%height=HEIGHT
    rect%fill_color=BACKGROUND_COLOR
    
    do y = 0, rows - 1
        do x = 0, cols - 1
            if (mod(x + y, 2) == 0) then
                color = ALTERNATE_COLOR
            else
                color = BACKGROUND_COLOR
            endif

            rect%x = x * CELL_WIDTH
            rect%y = y * CELL_HEIGHT
            rect%width = CELL_WIDTH
            rect%height = CELL_HEIGHT
            rect%fill_color = color

            call checker%add_shape(rect)
        end do
    end do
    
    call checker%save_to_file("svg")
    call checker%save_to_file("ppm")
    
    print *, 'Image successfully saved to checker.(ppm\svg)'
    
end program chess_checker
