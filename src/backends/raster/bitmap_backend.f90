module fig_bitmap
    use fig_canvas
    use fig_shapes
    use fig_drawing
    use fig_bitmap_circle
    use fig_bitmap_ellipse
    use fig_bitmap_rect
    use fig_bitmap_line
    use fig_bitmap_triangle
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
        integer, intent(in) :: width, height
        this%size%width=width
        this%size%height=height
        allocate(this%pixels(0:int(width)-1, 0:int(height)-1))
    end subroutine init_bitmap

    subroutine save_to_file(this,draw,file_path)
        class(bitmap_canvas), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        type(drawing), intent(in):: draw
        call this%apply_shapes(draw)

        call this%save_to_ppm(file_path)
    end subroutine save_to_file

    subroutine save_to_ppm(this,file_path)
        class(bitmap_canvas), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        integer :: unit_num, ierr
        integer :: i,j
        integer :: bytes(3)


        open(newunit=unit_num, file=file_path//'.ppm', status='replace', action='write', iostat=ierr)
        if (ierr /= 0) then
            print *, "Error opening file ", file_path
            stop
        endif

        write(unit_num, '(a2)') 'P6'
        write(unit_num, '(i0," ",i0)')  int(this%size%width), int(this%size%height)
        write(unit_num, '(i0)') 2**rgb_bit_depth-1
        do j = 0, int(this%size%height)-1
            do i = 0, int(this%size%width)-1
                bytes(1) = ibits(this%pixels(i, j), 0, rgb_bit_depth)
                bytes(2) = ibits(this%pixels(i, j), rgb_bit_depth, rgb_bit_depth)
                bytes(3) = ibits(this%pixels(i, j), 2*rgb_bit_depth, rgb_bit_depth)
        
                write(unit_num, '(3a1)', advance='no') bytes
            end do
        end do

        close(unit_num)
    end subroutine save_to_ppm

    subroutine bitmap_write_shape(canva,sh)
        class(bitmap_canvas), intent(inout) :: canva
        class(shape), intent(in) :: sh

        select type(sh)
        type is (circle)
            call write_circle(canva, canva%pixels, sh)
        type is (ellipse)
            call write_ellipse(canva ,canva%pixels,sh)
        type is (rectangle)
            call write_rectangle(canva ,canva%pixels,sh)
        type is (line)
            call write_line(canva ,canva%pixels,sh)
        type is (triangle)
            call write_triangle(canva ,canva%pixels,sh)
        end select
    end subroutine bitmap_write_shape

    subroutine apply_shapes(canva,draw)
        class(bitmap_canvas), intent(inout) :: canva
        type(drawing), intent(in):: draw
        integer :: i
        canva%pixels=0

        do i = 1, draw%shape_count
            call bitmap_write_shape(canva,draw%shapes(i)%sh)
        end do


    end subroutine apply_shapes

end module fig_bitmap

