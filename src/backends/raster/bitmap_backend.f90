module fig_bitmap
    use cairo
    use cairo_enums
    use cairo_types
    use cairo_extra
    use fig_drawing
    use fig_cairo
    implicit none
    type,extends(cairo_canvas) :: bitmap_canvas
    contains
        procedure :: init => init_bitmap
        procedure :: save_to_png
        procedure :: load_from_ppm
        procedure :: save_to_ppm
    end type bitmap_canvas
contains

    subroutine init_bitmap(this, width, height,title)
        class(bitmap_canvas), intent(inout) :: this
        integer, intent(in) :: width, height
        character(len=*), intent(in) :: title

        this%size%width=width
        this%size%height=height
        this%title=title

        this%surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,width, height)
        this%cairo = cairo_create(this%surface)
    end subroutine init_bitmap

    subroutine load_from_ppm(this,file_path)
        class(bitmap_canvas), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        integer :: unit_num, ierr, offset
        integer :: i, j, width, height, max_color_value
        integer :: red, green, blue
        character(len=2) :: magic_number
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

        call this%init(width,height,file_path)

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

    subroutine save_to_ppm(this)
        class(bitmap_canvas), intent(inout) :: this
        integer :: unit_num, ierr
        integer :: i,j
        integer :: bytes(3)
        integer :: pixel_t


        open(newunit=unit_num, file=trim(this%title)//".ppm", status='replace', action='write', iostat=ierr)
        if (ierr /= 0) then
            print *, "Error opening file ", trim(this%title)//'.ppm'
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

    subroutine save_to_png(this)
        class(bitmap_canvas), intent(inout) :: this
        integer :: r
        r = cairo_surface_write_to_png(this%surface, trim(this%title) // ".png" // c_null_char)
    end subroutine save_to_png


end module fig_bitmap

