module fig_svg
    use fig_svg_utils
    use fig_canvas
    use fig_shapes
    use fig_rgb
    implicit none
    private
    public :: svg_canvas ,save_to_svg
    type,extends(base_canvas) :: svg_canvas
    contains
        procedure :: save_to_file
    end type svg_canvas
contains
    subroutine save_to_file(this)
        class(svg_canvas), intent(inout) :: this
        call save_to_svg(this,this%shapes)
    end subroutine save_to_file

    subroutine save_to_svg(this,shapes)
        !! to be used in the general canvas
        class(base_canvas), intent(inout) :: this
        integer :: unit_num, ierr, i
        type(shapeWrapper), allocatable,intent(in) :: shapes(:)
        character(len=:), allocatable :: file_path
        character(len=:), allocatable :: s_line

        file_path=this%title // '.svg'
        open(newunit=unit_num, file=file_path, status='replace', action='write', iostat=ierr)
        if (ierr /= 0) then
            print *, "Error opening file ", file_path
            stop
        endif

        write(unit_num, '(A)') '<svg '&
             // attribute('width', trim(adjustl(real_to_str(this%width))), '') &
             // attribute('height', trim(adjustl(real_to_str(this%height))), '') &
             // attribute('xmlns', "http://www.w3.org/2000/svg", '') &
             //' >'
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

        res_str = '<circle ' &
            // attribute('cx', trim(adjustl(real_to_str(this%cx))), '') &
            // attribute('cy', trim(adjustl(real_to_str(this%cy))), '') &
            // attribute('r', trim(adjustl(real_to_str(this%r))), '') &
            // attribute('fill', trim(adjustl(rgb_to_string(this%fill_color))), '') &
            // attribute('stroke', trim(adjustl(rgb_to_string(this%stroke_color))), '') // '/>'
    end subroutine write_circle

    subroutine write_rectangle(this,res_str)
        type(rectangle), intent(in) :: this
        character(len=:), allocatable,intent(inout) :: res_str

        res_str = '<rect ' // attribute('x', trim(adjustl(real_to_str(this%x))), '') &
             // attribute('y', trim(adjustl(real_to_str(this%y))), '') &
             // attribute('width', trim(adjustl(real_to_str(this%width))), '') &
             // attribute('height', trim(adjustl(real_to_str(this%height))), '') &
             // attribute('fill', trim(adjustl(rgb_to_string(this%fill_color))), '') // '/>'
    end subroutine write_rectangle
    

    subroutine write_shape(sh,res_str)
        class(shape), intent(in) :: sh
        character(len=:), allocatable,intent(inout) :: res_str

        select type(sh)
        type is (circle)
            call write_circle(sh, res_str)
        type is (rectangle)
            call write_rectangle(sh, res_str)
        end select
    end subroutine write_shape

end module fig_svg

