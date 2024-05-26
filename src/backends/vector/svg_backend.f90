module fig_svg
    use fig_svg_utils
    use fig_canvas
    use fig_shapes
    use fig_rgb
    implicit none
    private
    public :: svg_canvas ,save_to_svg
    integer :: unit_num
    type,extends(base_canvas) :: svg_canvas
    contains
        procedure :: save_to_file
        procedure :: draw_shape => svg_write_shape
    end type svg_canvas
contains
    subroutine save_to_file(this)
        class(svg_canvas), intent(inout) :: this
        call save_to_svg(this,this%shapes)
    end subroutine save_to_file

    subroutine save_to_svg(this,shapes)
        class(svg_canvas), intent(inout) :: this
        integer :: ierr, i
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
            call svg_write_shape(this,shapes(i)%sh)
        end do

        write(unit_num, '(A)') '</svg>'
        close(unit_num)

    end subroutine save_to_svg

    subroutine write_circle(this)
        type(circle), intent(in) :: this

        write(unit_num, '(A)') '<circle ' &
            // attribute('cx', trim(adjustl(real_to_str(this%cx))), '') &
            // attribute('cy', trim(adjustl(real_to_str(this%cy))), '') &
            // attribute('r', trim(adjustl(real_to_str(this%r))), '') &
            // attribute('fill', trim(adjustl(rgb_to_string(this%fill_color))), '') &
            // attribute('stroke', trim(adjustl(rgb_to_string(this%stroke_color))), '') // '/>'
    end subroutine write_circle

    subroutine write_rectangle(this)
        type(rectangle), intent(in) :: this

        write(unit_num, '(A)') '<rect ' &
            // attribute('x', trim(adjustl(real_to_str(this%x))), '') &
            // attribute('y', trim(adjustl(real_to_str(this%y))), '') &
            // attribute('width', trim(adjustl(real_to_str(this%width))), '') &
            // attribute('height', trim(adjustl(real_to_str(this%height))), '') &
            // attribute('fill', trim(adjustl(rgb_to_string(this%fill_color))), '') // '/>'
    end subroutine write_rectangle
    

    subroutine svg_write_shape(canva,sh)
        class(svg_canvas), intent(inout) :: canva
        class(shape), intent(in) :: sh

        select type(sh)
        type is (circle)
            call write_circle(sh)
        type is (rectangle)
            call write_rectangle(sh)
        end select
    end subroutine svg_write_shape

end module fig_svg

