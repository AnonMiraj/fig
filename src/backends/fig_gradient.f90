module fig_gradient
    use fig_rgb
    use fig_rgb_color_constants
    implicit none

    type pattern_stop
        real(kind=8) :: offset=0
        type(RGB) :: stop_color = FIG_COLOR_BLANK
    end type pattern_stop

    type stops_a
        type(pattern_stop), allocatable:: stop_array(:)
        integer :: stop_count
        contains
        procedure :: init => init_stops_a
        procedure :: add_stop
        procedure :: clear_stops
    end type stops_a


    type,extends(pattern) :: linear_gradient
        real(kind=8) :: x1=0
        real(kind=8) :: y1=0
        real(kind=8) :: x2=0
        real(kind=8) :: y2=0
        type(stops_a) :: stops

    end type linear_gradient
    
    type,extends(pattern) :: radial_gradient
        real(kind=8) :: fx=0
        real(kind=8) :: fy=0
        real(kind=8) :: fr=0
        real(kind=8) :: cx=0
        real(kind=8) :: cy=0
        real(kind=8) :: cr=0

    end type radial_gradient
    contains
    subroutine init_stops_a(this)
        class(stops_a), intent(inout) :: this
        this%stop_count = 0
        allocate(this%stop_array(0))
    end subroutine init_stops_a


    subroutine add_stop(this, offset, color)
        class(stops_a), intent(inout) :: this
        real(kind=8), intent(in) :: offset
        type(RGB), intent(in) :: color
        type(pattern_stop), allocatable :: new_stop_array(:)

        ! Allocate new array for stops if not already allocated
        if (.not. allocated(this%stop_array)) then
            allocate(this%stop_array(1))
        else
            ! Allocate new array and copy existing stops
            allocate(new_stop_array(size(this%stop_array) + 1))
            new_stop_array(:size(this%stop_array)) = this%stop_array
            deallocate(this%stop_array)
            allocate(this%stop_array(size(new_stop_array)))
            this%stop_array = new_stop_array
        end if

        this%stop_count = this%stop_count + 1
        this%stop_array(this%stop_count)%offset = offset
        this%stop_array(this%stop_count)%stop_color = color
    end subroutine add_stop
    subroutine clear_stops(this)
        class(stops_a), intent(inout) :: this

        this%stop_count = 0
    end subroutine clear_stops
end module fig_gradient

