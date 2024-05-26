program circles_pattern
    !!use fig_g_canvas
    use fig_shapes
    use fig_rgb
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
    !!type(g_canvas) :: canva
    type(RGB) :: color
    type(RGB), dimension(8) :: color_palette
    type(circle) :: circ
    !!call canva%init(real(WIDTH), real(HEIGHT), "circles_pattern")
    
    color_palette = [ RED,   & 
                      MAGENTA, &
                      YELLOW, &
                      GREEN,   &
                      BLUE,   &
                      CYAN,  &
                      PINK, &
                      WHITE ]
    
    do y = 0, rows - 1
        do x = 0, cols - 1
            center_x = (x + 0.5) * CELL_WIDTH
            center_y = (y + 0.5) * CELL_HEIGHT
            
            t = (real(x) + real(y)) / real(cols + rows - 2)
            radius = int(min(CELL_WIDTH, CELL_HEIGHT) * lerpf(0.125, 0.5, t))
            
            circ%r=radius
            circ%cx=center_x
            circ%cy=center_y
            circ%fill_color=color_palette(mod(3*x+2*y,8)+1)
            circ%stroke_color=color_palette(mod(2*x+3*y,8)+1)
            !!call canva%add_shape(circ)
        end do
    end do

    !!call canva%save_to_file('ppm') 
    !!call canva%save_to_file('svg') 
    
     
    print *, 'Image successfully saved to circles_pattern.(ppm\svg)'
 
contains
    
    function lerpf(a, b, t)
        real :: lerpf
        real, intent(in) :: a, b, t
        lerpf = a + (b - a) * t
    end function lerpf
end program circles_pattern
