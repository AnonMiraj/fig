program test_fig_fill_triangle
    use fig_drawing
    use fig_shapes
    use fig_primitive
    use fig_rgb
    implicit none

    integer :: result
    type(drawing) :: test_canvas
    type(RGB), dimension(0:6) :: colors
    integer :: i, j, ind=0,ind1,ind2
    integer :: triangle_size = 50
    type(triangle) :: tri
    type(circle) :: circ


    call test_canvas%init( 801.0, 801.0, "cool_triangle_pattern")

    do i = 0, 6
        call random_color(colors(i))
    end do

    tri%stroke_color=FIG_COLOR_BLACK
    circ%stroke_color=FIG_COLOR_BLACK
    circ%r=5
    do i = 0, 800, triangle_size
        do j = 0, 800, triangle_size
            ind = mod(i + 2 * j, 7)
            ind1 = mod(ind * 2, 7)
            ind2 = mod(ind * 3, 7)

            tri%x1=i ;tri%y1= j
            tri%x2=i +triangle_size;tri%y2= j
            tri%x3=i +triangle_size;tri%y3= j +triangle_size
            tri%fill_color=colors(ind1)
            call test_canvas%add_shape(tri)

            tri%x2=i ;tri%y2= j+triangle_size
            tri%fill_color=colors(ind2)
            call test_canvas%add_shape(tri)

            circ%cx=i;circ%cy=j
            circ%fill_color=colors(ind1)
            call test_canvas%add_shape(circ)
            circ%cx=i + triangle_size
            call test_canvas%add_shape(circ)
            circ%cy=j + triangle_size
            call test_canvas%add_shape(circ)
            circ%cx=i
            call test_canvas%add_shape(circ)

            circ%cx=i+triangle_size/2
            circ%cy=j+triangle_size/2
            call test_canvas%add_shape(circ)
 
        end do
    end do

    call test_canvas%save_to_file('svg')
    call test_canvas%save_to_file('ppm')
    print *, "drawing exported successfully: cool_triangle_pattern.(ppm\svg)"
contains 
    subroutine random_color(color)
        type(RGB) :: color
        real :: r, g, b
        call random_number(r)
        call random_number(g)
        call random_number(b)
        color%r = int(r * 255)
        color%g = int(g * 255)
        color%b = int(b * 255)
        color%a = 255
    end subroutine random_color
end program test_fig_fill_triangle

