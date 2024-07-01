module fig_bitmap
    use plutovg
    use plutovg_extra
    use fig_canvas
    use fig_shapes
    use fig_drawing
    use fig_config
    use fig_bitmap_utils
    use fig_bitmap_circle
    use fig_bitmap_ellipse
    use fig_bitmap_line
    use fig_bitmap_rect
    use fig_bitmap_triangle
    use fig_rgb
    implicit none
    private
    public :: bitmap_canvas 
    type,extends(base_canvas) :: bitmap_canvas
        type(c_ptr) :: surface
        type(c_ptr) :: pluto
    contains
        procedure :: init => init_bitmap
        procedure :: destroy
        procedure :: save_to_file
        procedure :: load_from_ppm
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

        this%surface = plutovg_surface_create(width, height)
        this%pluto = plutovg_create(this%surface)
    end subroutine init_bitmap

    subroutine destroy(this)
        class(bitmap_canvas), intent(inout) :: this

        call plutovg_surface_destroy(this%surface)
        call plutovg_destroy(this%pluto)
    end subroutine destroy

    subroutine save_to_file(this,draw,file_path)
        class(bitmap_canvas), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        type(drawing), intent(in):: draw
        call this%apply_shapes(draw)

        call this%save_to_ppm(file_path)
    end subroutine save_to_file

    subroutine load_from_ppm(this,file_path)
        class(bitmap_canvas), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        integer :: unit_num, ierr, offset
        integer :: i, j, width, height, max_color_value
        integer :: red, green, blue
        character(len=2) :: magic_number
        integer :: bytes(3)
        character(len=1) :: temp
        character(len=1) :: byte
        character :: ccode

        open(newunit=unit_num, file=file_path, status='old', access="stream", form="formatted", iostat=ierr)
        if (ierr /= 0) then
            print *, "Error opening file ", file_path
            stop
        endif

        read(unit_num, '(a2)') magic_number

        if (magic_number /= 'P6') then
            print *, "Error reading magic number or not a P6 PPM file"
            stop
        endif

        read(unit_num, *) width, height, max_color_value

        if (max_color_value /= (2**rgb_bit_depth-1)) then
            print *, "Unsupported max color value: ", max_color_value
            stop
        endif

        inquire(unit_num, pos=offset)

        close(unit_num)

        call this%init(width,height)

        open(newunit=unit_num, file=file_path, access="stream", status="old")

        read(unit_num, pos=offset-1) ccode

        do j = 0, height - 1
            do i = 0, width - 1
            read(unit_num) ccode
            red = ichar(ccode)
            read(unit_num) ccode
            green = ichar(ccode)
            read(unit_num) ccode
            blue = ichar(ccode)
            call set_pixel(this%surface, i, j, blue + shiftl(green, 8) + shiftl(red, 16))
            end do
        end do

        close(unit_num)
    end subroutine load_from_ppm

    subroutine save_to_ppm(this,file_path)
        class(bitmap_canvas), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        integer :: unit_num, ierr
        integer :: i,j
        integer :: bytes(3)
        integer(pixel) :: pixel_t


        open(newunit=unit_num, file=trim(file_path)//'.ppm', status='replace', action='write', iostat=ierr)
        if (ierr /= 0) then
            print *, "Error opening file ", trim(file_path)//'.ppm'
            stop
        endif

        write(unit_num, '(a2)') 'P6'
        write(unit_num, '(i0," ",i0)')  int(this%size%width), int(this%size%height)
        write(unit_num, '(i0)') 2**rgb_bit_depth-1
        do j = 0, int(this%size%height)-1
            do i = 0, int(this%size%width)-1
                pixel_t = get_pixel(this%surface,i,j)
                bytes(3) = ibits(pixel_t, 0, rgb_bit_depth)
                bytes(2) = ibits(pixel_t, rgb_bit_depth, rgb_bit_depth)
                bytes(1) = ibits(pixel_t, 2*rgb_bit_depth, rgb_bit_depth)
        
                write(unit_num, '(3a1)', advance='no') bytes
            end do
        end do

        close(unit_num)
    end subroutine save_to_ppm

    subroutine bitmap_write_shape(canva,sh)
        class(bitmap_canvas), intent(inout) :: canva
        class(shape), intent(in) :: sh
        type(canvas_point) :: p,p2,p3

        call plutovg_save(canva%pluto)
        select type(sh)
        type is (circle)
            call write_circle(canva, canva%pluto, sh)
        type is (ellipse)
            call write_ellipse(canva, canva%pluto, sh)
        type is (rectangle)
            call write_rectangle(canva, canva%pluto, sh)
        type is (line)
            call write_line(canva, canva%pluto, sh)
        type is (triangle)
            call write_triangle(canva, canva%pluto, sh)
        end select
        call plutovg_restore(canva%pluto)

    end subroutine bitmap_write_shape

    subroutine apply_shapes(canva,draw)
        class(bitmap_canvas), intent(inout) :: canva
        type(drawing), intent(in):: draw
        integer :: i
        type(rectangle) :: bg

        bg%height=canva%size%height
        bg%width=canva%size%width
        bg%upper_left%x=0
        bg%upper_left%y=0
        bg%fill_color=draw%background 
        call write_rectangle(canva ,canva%pluto,bg)

        do i = 1, draw%shape_count
            call bitmap_write_shape(canva,draw%shapes(i)%sh)
        end do


    end subroutine apply_shapes

end module fig_bitmap

