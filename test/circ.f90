program circles_pattern
    use fig_canvas
    use fig_rgb
    use fig_primitive
    implicit none
    integer, parameter :: WIDTH = 800
    integer, parameter :: HEIGHT = 600
    integer, parameter :: cols = 8*2
    integer, parameter :: rows = 6*2
    integer, parameter :: CELL_WIDTH = (WIDTH/cols)
    integer, parameter :: CELL_HEIGHT =(HEIGHT/rows)
    integer :: result,x,y,center_x, center_y
    real :: u, v, t,shade
    integer :: radius
    type(canvas) :: circ
    type(RGB) :: color
    type(RGB), dimension(8) :: color_palette ! Define a color palette
    call canvas_init(circ, WIDTH, HEIGHT, "circles_pattern")
    
    call fig_fill(circ, BLACK)
    color_palette = [ RED,   & ! Red
                      MAGENTA, & ! Orange
                      YELLOW, & ! Yellow
                      GREEN,   & ! Green
                      BLUE,   & ! Blue
                      CYAN,  & ! Indigo
                      PINK, & ! Purple
                      WHITE ] ! Pink
    
    ! Draw circles

! Draw circles
    do y = 0, rows - 1
        do x = 0, cols - 1
            center_x = (x + 0.5) * CELL_WIDTH
            center_y = (y + 0.5) * CELL_HEIGHT
            
            t = (real(x) + real(y)) / real(cols + rows - 2)
            radius = int(min(CELL_WIDTH, CELL_HEIGHT) * lerpf(0.125, 0.5, t))
            
            call fig_fill_circle(circ, center_x, center_y, radius, color_palette(mod(3*x+2*y,8)+1))
        end do
    end do

    call fig_save_to_ppm_file(circ, result)
     
    if (result == 0) then
        print *, 'Image successfully saved to circles_pattern.ppm'
    else
        print *, 'Error occurred while saving the image'
    end if
 
contains
    
    function lerpf(a, b, t)
        real :: lerpf
        real, intent(in) :: a, b, t
        lerpf = a + (b - a) * t
    end function lerpf
end program circles_pattern
