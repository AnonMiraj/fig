module fig_drawing
    use fig_config
    use fig_shapes
    use fig_svg
    use fig_bitmap
    implicit none
 
    type :: drawing
        real :: width, height
        character(len=:), allocatable :: title
        type(shapeWrapper), allocatable :: shapes(:)
        integer :: shape_count
    contains
        procedure :: add_shape
        procedure :: init
        procedure :: save_to_file
    end type drawing


contains

    subroutine init(this, width, height, fname)
        class(drawing), intent(inout) :: this
        real, intent(in) :: width, height
        character(len=*), intent(in) :: fname

        this%title = fname
        this%width = width
        this%height = height

        this%shape_count = 1
        allocate(this%shapes(1))
    end subroutine init

    subroutine add_shape(this, s)
        class(drawing), intent(inout) :: this
        class(shape), intent(in), target :: s
        integer :: new_size, i
        type(shapeWrapper), allocatable :: temp(:)

        if (this%shape_count >= size(this%shapes)) then
            new_size = 2 * size(this%shapes)

            allocate(temp(this%shape_count))
            temp = this%shapes(1:this%shape_count)
            
            deallocate(this%shapes)
            allocate(this%shapes(new_size))
            
            this%shapes(1:this%shape_count) = temp
            deallocate(temp)
        endif

        this%shape_count = this%shape_count + 1
        allocate(this%shapes(this%shape_count)%sh, source=s)
    end subroutine add_shape

   subroutine save_to_file(this,output_ext)
        class(drawing), intent(inout) :: this
        character(len=*), intent(in) :: output_ext
        type(bitmap_canvas):: temp_bitmap_canvas
        type(svg_canvas):: temp_svg_canvas

        select case (trim(output_ext))
        case ('svg')
            call temp_svg_canvas%init(this%width,this%height)
            call temp_svg_canvas%save_to_svg(this%shapes,this%title // '.svg')
        case default
            call temp_bitmap_canvas%init(this%width,this%height)
            call temp_bitmap_canvas%apply_shapes(this%shapes)
            call temp_bitmap_canvas%save_to_file(this%title // '.ppm')
        end select
    end subroutine save_to_file


end module fig_drawing

