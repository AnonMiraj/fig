module fig_bitmap
    use fig_canvas
    use fig_shapes
    use fig_config
    use fig_bitmap_utils
    use fig_rgb
    implicit none
    private
    public :: bitmap_canvas 
    type,extends(base_canvas) :: bitmap_canvas
        integer(pixel), dimension(:,:), allocatable:: pixels
    contains
        procedure :: init => init_bitmap
        procedure :: save_to_file
        procedure :: save_to_ppm
        procedure :: apply_shapes
        procedure :: draw_shape=> bitmap_write_shape
    end type bitmap_canvas
contains

    subroutine init_bitmap(this, width, height)
        class(bitmap_canvas), intent(inout) :: this
        real, intent(in) :: width, height
        call init(this,width, height)
        allocate(this%pixels(0:int(width)-1, 0:int(height)-1))
    end subroutine init_bitmap

    subroutine save_to_file(this,file_path)
        class(bitmap_canvas), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        call this%save_to_ppm(file_path)
    end subroutine save_to_file

    subroutine save_to_ppm(this,file_path)
        class(bitmap_canvas), intent(inout) :: this
        integer :: unit_num, ierr
        character(len=*), intent(in) :: file_path
        integer :: i,j
        integer :: bytes(3)


        open(newunit=unit_num, file=file_path, status='replace', action='write', iostat=ierr)
        if (ierr /= 0) then
            print *, "Error opening file ", file_path
            stop
        endif

        write(unit_num, '(a2)') 'P6'
        write(unit_num, '(i0," ",i0)')  int(this%width), int(this%height)
        write(unit_num, '(i0)') 2**rgb_bit_depth-1
        do j = 0, int(this%height)-1
            do i = 0, int(this%width)-1
                bytes(1) = ibits(this%pixels(i, j), 0, rgb_bit_depth)
                bytes(2) = ibits(this%pixels(i, j), rgb_bit_depth, rgb_bit_depth)
                bytes(3) = ibits(this%pixels(i, j), 2*rgb_bit_depth, rgb_bit_depth)
        
                write(unit_num, '(3a1)', advance='no') bytes
            end do
        end do

        close(unit_num)
    end subroutine save_to_ppm

    subroutine write_circle(canva,circ)
        type(circle), intent(in) :: circ
        class(bitmap_canvas), intent(inout) :: canva
        integer(pixel) :: stroke_color 
        integer(pixel) :: fill_color 
        integer :: x, y, d

        stroke_color = rgb_to_int(circ%stroke_color)
        fill_color = rgb_to_int(circ%fill_color)
        x = 0
        y = int(circ%r)
        d = 1 - int(circ%r)

        do while (x <= y)
            if (d < 0) then
                d = d + 2 * x + 3
            else
                d = d + 2 * (x - y) + 5
                y = y - 1
            end if
            x = x + 1


            call fill_rect(canva, canva%pixels, int(circ%cx) - x, int(circ%cy) + y-1, 2*x, 1, fill_color)
            call fill_rect(canva, canva%pixels, int(circ%cx) - x, int(circ%cy) - y, 2*x, 1, fill_color)
            call fill_rect(canva, canva%pixels, int(circ%cx) - y, int(circ%cy) + x-1, 2*y, 1, fill_color)
            call fill_rect(canva, canva%pixels, int(circ%cx) - y, int(circ%cy) - x, 2*y, 1, fill_color)   
        end do                    
                                  
        call fill_rect(canva, canva%pixels, int (circ%cx) - int(circ%r), int(circ%cy), 2*int(circ%r), 1, fill_color)
        !! draw stroke            
        !! TODO it is a bit messy for now 
        !! I need to think of a better design and handle stroke_widht somehow

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

            call draw_pixel(canva, canva%pixels, int(circ%cx) + x, int(circ%cy) + y, stroke_color)
            call draw_pixel(canva, canva%pixels, int(circ%cx) - x, int(circ%cy) + y, stroke_color)
            call draw_pixel(canva, canva%pixels, int(circ%cx) + x, int(circ%cy) - y, stroke_color)
            call draw_pixel(canva, canva%pixels, int(circ%cx) - x, int(circ%cy) - y, stroke_color)
            call draw_pixel(canva, canva%pixels, int(circ%cx) + y, int(circ%cy) + x, stroke_color)
            call draw_pixel(canva, canva%pixels, int(circ%cx) - y, int(circ%cy) + x, stroke_color)
            call draw_pixel(canva, canva%pixels, int(circ%cx) + y, int(circ%cy) - x, stroke_color)
            call draw_pixel(canva, canva%pixels, int(circ%cx) - y, int(circ%cy) - x, stroke_color)
        end do

        call draw_pixel(canva, canva%pixels, int(circ%cx) , int(circ%cy - circ%r), stroke_color)
        call draw_pixel(canva, canva%pixels, int(circ%cx) , int(circ%cy + circ%r), stroke_color)
        call draw_pixel(canva, canva%pixels, int(circ%cx - circ%r), int(circ%cy) , stroke_color)
        call draw_pixel(canva, canva%pixels, int(circ%cx + circ%r), int(circ%cy) , stroke_color)

    end subroutine write_circle

    subroutine write_rectangle(canva,rect)
        class(bitmap_canvas), intent(inout) :: canva
        type(rectangle), intent(in) :: rect
        integer(pixel) :: color
        
        color = rgb_to_int(rect%fill_color)
        call fill_rect(canva, canva%pixels, int(rect%x), int(rect%y), int(rect%width), int(rect%height), color)
        

    end subroutine write_rectangle
    
    subroutine bitmap_write_shape(canva,sh)
        class(bitmap_canvas), intent(inout) :: canva
        class(shape), intent(in) :: sh

        select type(sh)
        type is (circle)
            call write_circle(canva,sh)
        type is (rectangle)
            call write_rectangle(canva,sh)
        end select
    end subroutine bitmap_write_shape

    subroutine apply_shapes(canva,shapes,shape_count)
        class(bitmap_canvas), intent(inout) :: canva
        type(shapeWrapper), allocatable,intent(in) :: shapes(:)
        integer, intent(in) :: shape_count
        integer :: i

        do i = 1, shape_count
            call bitmap_write_shape(canva,shapes(i)%sh)
        end do


    end subroutine apply_shapes

end module fig_bitmap

