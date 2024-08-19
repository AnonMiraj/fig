module fig_drawing
    use fig_config
    use fig_shapes
    use fig_rgb
    use fig_rgb_color_constants
    implicit none

    type,private :: shape_wrapper
      class(shape), allocatable :: sh
    end type
 
    type :: drawing
        type(shape_wrapper), allocatable :: shapes(:)
        type(RGB) :: background = FIG_COLOR_BLANK
        integer :: shape_count
    contains
        procedure :: add_shape
        procedure :: init
    end type drawing


contains

    subroutine init(this)
        class(drawing), intent(inout) :: this
        this%shape_count = 0
        allocate(this%shapes(0))
    end subroutine init

    subroutine add_shape(this, s)
        class(drawing), intent(inout) :: this
        class(shape), intent(in), target :: s
        integer :: new_size
        type(shape_wrapper), allocatable :: temp(:)

        if (this%shape_count >= size(this%shapes)) then
            new_size = max(1, 2 * size(this%shapes))

            if (this%shape_count > 0) then
                allocate(temp(this%shape_count))
                temp = this%shapes(1:this%shape_count)
            endif
            
            deallocate(this%shapes)
            allocate(this%shapes(new_size))
            
            if (this%shape_count > 0) then
                this%shapes(1:this%shape_count) = temp
                deallocate(temp)
            endif

        endif

        this%shape_count = this%shape_count + 1
        allocate(this%shapes(this%shape_count)%sh, source=s)
    end subroutine add_shape

end module fig_drawing

