module svg_module
    use xml_helpers
    use fig_canvas
    implicit none
    private
    public :: svg_canvas, shape, circle, rectangle

    type, abstract :: shape
        character(len=:), allocatable :: fill_color
    contains
        procedure(write_shape), deferred :: write
    end type shape

    abstract interface
        subroutine write_shape(this, unit_num)
            import :: shape
            class(shape), intent(in) :: this
            integer, intent(in) :: unit_num
        end subroutine write_shape
    end interface

    type, extends(shape) :: circle
        real :: cx, cy, r
    contains
        procedure :: write => write_circle
    end type circle

    type, extends(shape) :: rectangle
        real :: x, y, width, height
    contains
        procedure :: write => write_rectangle
    end type rectangle

    type,extends(Tcanvas) :: svg_canvas
        type(shapeWrapper), allocatable :: shapes(:)
        integer :: shape_count
    contains
        procedure :: init => init_svg
        procedure :: add_shape
        procedure :: save_to_file
    end type svg_canvas

    type :: shapeWrapper
      class(shape), allocatable :: sh
    end type

contains

    subroutine init_svg(this, fname)
        class(svg_canvas), intent(inout) :: this
        character(len=*), intent(in) :: fname

        this%title = fname
        this%shape_count = 0
        allocate(this%shapes(0))
    end subroutine init_svg

    subroutine add_shape(this, s)
        class(svg_canvas), intent(inout) :: this
        class(shape), intent(in) :: s
        type(shapeWrapper), allocatable :: temp_shapes(:)
        this%shape_count = this%shape_count + 1
        call move_alloc(this%shapes, temp_shapes)
        allocate(this%shapes(this%shape_count))
        this%shapes(1:this%shape_count-1) = temp_shapes
        this%shapes(this%shape_count)%sh = s
    end subroutine add_shape

    subroutine save_to_file(this)
        class(svg_canvas), intent(inout) :: this
        integer :: unit_num, ierr, i

        open(newunit=unit_num, file=this%title, status='replace', action='write', iostat=ierr)
        if (ierr /= 0) then
            print *, "Error opening file ", this%title
            stop
        endif

        write(unit_num, '(A)') '<svg '&
             // attribute('width', trim(adjustl(real_to_str(this%width))), '') &
             // attribute('height', trim(adjustl(real_to_str(this%height))), '') //' >'
        do i = 1, this%shape_count
            call this%shapes(i)%sh%write(unit_num)
        end do

        write(unit_num, '(A)') '</svg>'
        close(unit_num)
    end subroutine save_to_file

    subroutine write_circle(this, unit_num)
        class(circle), intent(in) :: this
        integer, intent(in) :: unit_num

        write(unit_num, '(A)') '<circle ' // attribute('cx', trim(adjustl(real_to_str(this%cx))), '') &
             // attribute('cy', trim(adjustl(real_to_str(this%cy))), '') // attribute('r', trim(adjustl(real_to_str(this%r))), '') &
             // attribute('fill', trim(adjustl(this%fill_color)), '') // '/>'
    end subroutine write_circle

    subroutine write_rectangle(this, unit_num)
        class(rectangle), intent(in) :: this
        integer, intent(in) :: unit_num

        write(unit_num, '(A)') '<rect ' // attribute('x', trim(adjustl(real_to_str(this%x))), '') &
             // attribute('y', trim(adjustl(real_to_str(this%y))), '') &
             // attribute('width', trim(adjustl(real_to_str(this%width))), '') &
             // attribute('height', trim(adjustl(real_to_str(this%height))), '') &
             // attribute('fill', trim(adjustl(this%fill_color)), '') // '/>'
    end subroutine write_rectangle

end module svg_module

