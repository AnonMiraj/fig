program chess_checker
    use fig_canvas
    use fig_rgb
    use fig_primitive
    implicit none
    integer, parameter :: WIDTH = 800
    integer, parameter :: HEIGHT = 800
    integer, parameter :: cols = 8
    integer, parameter :: rows = 8
    integer, parameter :: CELL_WIDTH = (WIDTH/cols)
    integer, parameter :: CELL_HEIGHT =(HEIGHT/rows)
    integer :: result,x,y

    type(canvas) :: checker
    type(RGB) ::ALTERNATE_COLOR,BACKGROUND_COLOR, color
    call canvas_init(checker, WIDTH, HEIGHT, "checker")
    
    BACKGROUND_COLOR = BLACK 
    ALTERNATE_COLOR = WHITE 
    call fig_fill(checker, BACKGROUND_COLOR)
    
    do y = 0, HEIGHT / CELL_HEIGHT
        do x = 0, WIDTH / CELL_WIDTH
            if (mod(x + y, 2) == 0) then
                color = ALTERNATE_COLOR
            else
                color = BACKGROUND_COLOR
            endif
            call fig_fill_rect(checker, (x - 1) * CELL_WIDTH, (y - 1) * CELL_HEIGHT, CELL_WIDTH, CELL_HEIGHT, color)
        end do
    end do
    
    call fig_save_to_ppm_file(checker, result)
    
    if (result == 0) then
        print *, 'Image successfully saved to checker.ppm'
    else
        print *, 'Error occurred while saving the image'
    end if
    
end program chess_checker
