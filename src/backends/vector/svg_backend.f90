module fig_svg
    use fig_utils
    use fig_canvas
    use fig_shapes
    implicit none
    private
    public :: svg_canvas 
    type,extends(Tcanvas) :: svg_canvas
    contains
        procedure :: init => init_svg
        procedure :: save_to_file
    end type svg_canvas
contains

    subroutine init_svg(this, fname)
        class(svg_canvas), intent(inout) :: this
        character(len=*), intent(in) :: fname

        this%title = fname
        this%shape_count = 0
        allocate(this%shapes(0))
    end subroutine init_svg

    subroutine save_to_file(this)
        class(svg_canvas), intent(inout) :: this
        call save_to_svg(this,this%shapes)
    end subroutine save_to_file

    subroutine save_to_svg(this,shapes)
        !! to be used in the general canvas
        class(Tcanvas), intent(inout) :: this
        integer :: unit_num, ierr, i
        type(shapeWrapper), allocatable,intent(in) :: shapes(:)
        character(len=:), allocatable :: s_line

        open(newunit=unit_num, file=this%title, status='replace', action='write', iostat=ierr)
        if (ierr /= 0) then
            print *, "Error opening file ", this%title
            stop
        endif

        write(unit_num, '(A)') '<svg '&
             // attribute('width', trim(adjustl(real_to_str(this%width))), '') &
             // attribute('height', trim(adjustl(real_to_str(this%height))), '') //' >'
        do i = 1, this%shape_count
            call write_shape(shapes(i)%sh,s_line)
            write(unit_num, '(A)') s_line
        end do

        write(unit_num, '(A)') '</svg>'
        close(unit_num)

    end subroutine save_to_svg

    subroutine write_circle(this,res_str)
        type(circle), intent(in) :: this
        character(len=:), allocatable,intent(inout) :: res_str

        res_str = '<circle ' // attribute('cx', trim(adjustl(real_to_str(this%cx))), '') &
             // attribute('cy', trim(adjustl(real_to_str(this%cy))), '') // attribute('r', trim(adjustl(real_to_str(this%r))), '') &
             // attribute('fill', trim(adjustl(this%fill_color)), '') // '/>'
    end subroutine write_circle

    subroutine write_rectangle(this,res_str)
        type(rectangle), intent(in) :: this
        character(len=:), allocatable,intent(inout) :: res_str

        res_str = '<rect ' // attribute('x', trim(adjustl(real_to_str(this%x))), '') &
             // attribute('y', trim(adjustl(real_to_str(this%y))), '') &
             // attribute('width', trim(adjustl(real_to_str(this%width))), '') &
             // attribute('height', trim(adjustl(real_to_str(this%height))), '') &
             // attribute('fill', trim(adjustl(this%fill_color)), '') // '/>'
    end subroutine write_rectangle
    

    subroutine write_shape(this,res_str)
        class(shape), intent(in) :: this
        character(len=:), allocatable,intent(inout) :: res_str

        select type(this)
        type is (circle)
            call write_circle(this, res_str)
        type is (rectangle)
            call write_rectangle(this, res_str)
        end select
    end subroutine write_shape

end module fig_svg

