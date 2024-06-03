module fig_svg
    use fig_svg_shapes
    use fig_canvas
    use fig_shapes
    use fig_rgb
    implicit none
    private
    public :: svg_canvas ,save_to_svg
    integer :: unit_num
    type,extends(base_canvas) :: svg_canvas
    contains
        procedure :: save_to_svg
        procedure :: draw_shape => svg_write_shape
    end type svg_canvas
contains

    subroutine save_to_svg(this,shapes,shape_count,file_path)
        class(svg_canvas), intent(inout) :: this
        type(shapeWrapper), allocatable,intent(in) :: shapes(:)
        integer, intent(in) :: shape_count
        character(len=*), intent(in) :: file_path
        integer :: ierr, i

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
        do i = 1, shape_count
            call svg_write_shape(this,shapes(i)%sh)
        end do

        write(unit_num, '(A)') '</svg>'
        close(unit_num)

    end subroutine save_to_svg
    subroutine svg_write_shape(canva,sh)
        class(svg_canvas), intent(inout) :: canva
        class(shape), intent(in) :: sh

        select type(sh)
        type is (circle)
            call write_circle(sh, unit_num)
        type is (ellipse)
            call write_ellipse(sh, unit_num)
        type is (rectangle)
            call write_rectangle(sh, unit_num)
        end select
    end subroutine svg_write_shape

end module fig_svg

