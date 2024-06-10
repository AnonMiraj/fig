module fig_svg
    use fig_svg_shapes
    use fig_canvas
    use fig_shapes
    use fig_drawing
    use fig_rgb
    implicit none
    private
    public :: svg_canvas ,save_to_file
    integer :: unit_num
    type,extends(base_canvas) :: svg_canvas
    contains
        procedure :: save_to_file
        procedure :: draw_shape => svg_write_shape
    end type svg_canvas
contains

    subroutine save_to_file(this,draw,file_path)
        class(svg_canvas), intent(inout) :: this
        type(drawing), intent(in):: draw
        character(len=*), intent(in) :: file_path
        type(rectangle) :: bg
        integer :: ierr, i

        open(newunit=unit_num, file=file_path//".svg", status='replace', action='write', iostat=ierr)
        if (ierr /= 0) then
            print *, "Error opening file ", file_path
            stop
        endif

        write(unit_num, '(A)') '<svg '&
             // attribute('width', trim(adjustl(int_to_str(this%size%width))), '') &
             // attribute('height', trim(adjustl(int_to_str(this%size%height))), '') &
             // attribute('xmlns', "http://www.w3.org/2000/svg", '') &
             //' >'

        bg%height=this%size%height
        bg%width=this%size%height
        bg%upper_left%x=0
        bg%upper_left%y=0
        bg%fill_color=draw%background
        call svg_write_shape(this,bg)
        do i = 1, draw%shape_count
            call svg_write_shape(this,draw%shapes(i)%sh)
        end do

        write(unit_num, '(A)') '</svg>'
        close(unit_num)

    end subroutine save_to_file

    subroutine svg_write_shape(canva,sh)
        class(svg_canvas), intent(inout) :: canva
        class(shape), intent(in) :: sh

        select type(sh)
        type is (circle)
            call write_circle(sh, canva%size, unit_num)
        type is (ellipse)
            call write_ellipse(sh, canva%size, unit_num)
        type is (rectangle)
            call write_rectangle(sh, canva%size, unit_num)
        type is (triangle)
            call write_triangle(sh, canva%size, unit_num)
        type is (line)
            call write_line(sh, canva%size, unit_num)
        end select
    end subroutine svg_write_shape

end module fig_svg

