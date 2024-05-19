module fig_bitmap
    use fig_utils
    use fig_canvas
    use fig_shapes
    use fig_config
    use fig_rgb
    implicit none
    private
    public :: bitmap_canvas 
    type,extends(Tcanvas) :: bitmap_canvas
        integer(pixel), dimension(:,:), allocatable:: pixels
    contains
        procedure :: init => init_bitmap
        procedure :: save_to_file
        procedure :: save_to_ppm
        procedure :: apply_shapes
    end type bitmap_canvas
contains

    subroutine init_bitmap(this, width, height , fname)
        class(bitmap_canvas), intent(inout) :: this
        real, intent(in) :: width, height
        character(len=*), intent(in) :: fname
        call this%Tcanvas%init(width, height, fname)

        allocate(this%pixels(0:int(width)-1, 0:int(height)-1))
    end subroutine init_bitmap

    subroutine save_to_file(this)
        class(bitmap_canvas), intent(inout) :: this
        call this%apply_shapes()
        call this%save_to_ppm()
    end subroutine save_to_file

    subroutine save_to_ppm(this)
        class(bitmap_canvas), intent(inout) :: this
        integer :: unit_num, ierr
        character(len=:), allocatable :: file_path
        integer :: i,j
        integer :: bytes(3)

        file_path=this%title // '.ppm'


        open(newunit=unit_num, file=file_path, status='replace', action='write', iostat=ierr)
        if (ierr /= 0) then
            print *, "Error opening file ", file_path
            stop
        endif


        write(unit_num, '(a2)') 'P6'
        write(unit_num, '(i0," ",i0)')  int(this%width), int(this%height)
        write(unit_num, '(i0)') 2**rgb_bit_depth-1
        do j = 0, this%height-1
            do i = 0, this%width-1
                bytes(1) = ibits(this%pixels(i, j), 0, rgb_bit_depth)
                bytes(2) = ibits(this%pixels(i, j), rgb_bit_depth, rgb_bit_depth)
                bytes(3) = ibits(this%pixels(i, j), 2*rgb_bit_depth, rgb_bit_depth)
        
                write(unit_num, '(3a1)', advance='no') bytes
            end do
        end do

        close(unit_num)
    end subroutine save_to_ppm

    subroutine fig_draw_pixel_i(canva, x, y, color)
        !! very temporary
        type(bitmap_canvas), intent(inout) :: canva
        integer, intent(in) :: x, y
        integer(pixel), intent(in) :: color
    
        if (x >= 0 .and. x < int(canva%width) .and. y >= 0 .and. y < int(canva%height)) then
            canva%pixels(x, y) = color
        end if
    end subroutine fig_draw_pixel_i


    subroutine write_circle(circ,canva)
        !! TODO will implement fill later when i do lines
        type(circle), intent(in) :: circ
        class(bitmap_canvas), intent(inout) :: canva
        integer(pixel) :: color 
        integer :: x, y, d

        color = rgb_to_int(circ%fill_color)
        x = 0
        y = int(circ%r)
        d = 1 - int(circ%r)

        do while (x < y)
            if (d < 0) then
                d = d + 2 * x + 3
            else
                d = d + 2 * (x - y) + 5
                y = y - 1
            end if
            x = x + 1

            call fig_draw_pixel_i(canva, int(circ%cx) + x, int(circ%cy) + y, color)
            call fig_draw_pixel_i(canva, int(circ%cx) - x, int(circ%cy) + y, color)
            call fig_draw_pixel_i(canva, int(circ%cx) + x, int(circ%cy) - y, color)
            call fig_draw_pixel_i(canva, int(circ%cx) - x, int(circ%cy) - y, color)
            call fig_draw_pixel_i(canva, int(circ%cx) + y, int(circ%cy) + x, color)
            call fig_draw_pixel_i(canva, int(circ%cx) - y, int(circ%cy) + x, color)
            call fig_draw_pixel_i(canva, int(circ%cx) + y, int(circ%cy) - x, color)
            call fig_draw_pixel_i(canva, int(circ%cx) - y, int(circ%cy) - x, color)
        end do

        call fig_draw_pixel_i(canva, int(circ%cx) , int(circ%cy - circ%r), color)
        call fig_draw_pixel_i(canva, int(circ%cx) , int(circ%cy + circ%r), color)
        call fig_draw_pixel_i(canva, int(circ%cx - circ%r), int(circ%cy) , color)
        call fig_draw_pixel_i(canva, int(circ%cx + circ%r), int(circ%cy) , color)
    end subroutine write_circle

    subroutine write_rectangle(rect,canva)
        type(rectangle), intent(in) :: rect
        class(bitmap_canvas), intent(inout) :: canva

        integer(pixel) :: color
        integer :: x, y
        integer :: x_start, y_start
        integer :: x_end, y_end
        
        color = rgb_to_int(rect%fill_color)
        
        x_start = max(int(rect%x),0)
        y_start = max(int(rect%y),0)
        x_end = int(min(rect%x + rect%width, canva%width))-1
        y_end = int(min(rect%y + rect%height, canva%height))-1
        
        do y = y_start, y_end 
            do x = x_start, x_end 
                canva%pixels(x, y) = color
            end do
        end do

    end subroutine write_rectangle
    

    subroutine write_shape(sh,canva)
        class(shape), intent(in) :: sh
        class(bitmap_canvas), intent(inout) :: canva

        select type(sh)
        type is (circle)
            call write_circle(sh, canva)
        type is (rectangle)
            call write_rectangle(sh, canva)
        end select
    end subroutine write_shape

    subroutine apply_shapes(canva)
        class(bitmap_canvas), intent(inout) :: canva
        integer :: i

        do i = 1, canva%shape_count
            call write_shape(canva%shapes(i)%sh,canva)
        end do


    end subroutine apply_shapes

end module fig_bitmap

