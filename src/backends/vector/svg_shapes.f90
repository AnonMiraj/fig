module fig_svg_shapes
    use fig_shapes
    use fig_svg_utils
contains

    subroutine write_circle(sh,sz,unit_num)
        type(circle), intent(in) :: sh
        type(canvas_size),intent(in) :: sz
        integer,intent(in) :: unit_num
        type(canvas_point) :: c

        c=to_canvas(sh%center,sz)

        write(unit_num, '(A)') '<circle ' &
            // attribute('cx', trim(adjustl(int_to_str(c%x))), '') &
            // attribute('cy', trim(adjustl(int_to_str(c%y))), '') &
            // attribute('r', trim(adjustl(real_to_str(sh%r))), '') &
            // attribute('fill', trim(adjustl(rgb_to_string(sh%fill_color))), '') &
            // '/>'
    end subroutine write_circle

    subroutine write_ellipse(sh,sz,unit_num)
        type(ellipse), intent(in) :: sh
        type(canvas_size),intent(in) :: sz
        integer,intent(in) :: unit_num
        type(canvas_point) :: c
        
        c=to_canvas(sh%center,sz)

        write(unit_num, '(A)') '<ellipse ' &
            // attribute('cx', trim(adjustl(int_to_str(c%x))), '') &
            // attribute('cy', trim(adjustl(int_to_str(c%y))), '') &
            // attribute('rx', trim(adjustl(real_to_str(sh%rx))), '') &
            // attribute('ry', trim(adjustl(real_to_str(sh%ry))), '') &
            // attribute('fill', trim(adjustl(rgb_to_string(sh%fill_color))), '') &
            // '/>'
    end subroutine write_ellipse


    subroutine write_rectangle(sh,sz,unit_num)
        type(rectangle), intent(in) :: sh
        type(canvas_size),intent(in) :: sz
        integer,intent(in) :: unit_num
        type(canvas_point) :: p
        
        p=to_canvas(sh%upper_left,sz)

        write(unit_num, '(A)') '<rect ' &
            // attribute('x', trim(adjustl(int_to_str(p%x))), '') &
            // attribute('y', trim(adjustl(int_to_str(p%y))), '') &
            // attribute('width', trim(adjustl(real_to_str(sh%width))), '') &
            // attribute('height', trim(adjustl(real_to_str(sh%height))), '') &
            // attribute('fill', trim(adjustl(rgb_to_string(sh%fill_color))), '') &
            // attribute('stroke', trim(adjustl(rgb_to_string(sh%stroke_color))), '')&
            // '/>'
    end subroutine write_rectangle

    subroutine write_line(sh,sz,unit_num)
        type(line), intent(in) :: sh
        type(canvas_size),intent(in) :: sz
        integer, intent(in) :: unit_num
        type(canvas_point) :: p1,p2
        
        p1=to_canvas(sh%p1,sz)
        p2=to_canvas(sh%p2,sz)

        write(unit_num, '(A)') '<line ' &
            // attribute('x1', trim(adjustl(int_to_str(p1%x))), '') &
            // attribute('y1', trim(adjustl(int_to_str(p1%y))), '') &
            // attribute('x2', trim(adjustl(int_to_str(p2%x))), '') &
            // attribute('y2', trim(adjustl(int_to_str(p2%y))), '') &
            // attribute('stroke', trim(adjustl(rgb_to_string(sh%stroke_color))), '')&
            // attribute('stroke-width', trim(adjustl(int_to_str(sh%stroke_width))), '')&
            //'/>'
    end subroutine write_line


    subroutine write_triangle(sh,sz,unit_num)
        type(triangle), intent(in) :: sh
        type(canvas_size),intent(in) :: sz
        integer, intent(in) :: unit_num
        type(canvas_point) :: p1,p2,p3
        
        p1=to_canvas(sh%p1,sz)
        p2=to_canvas(sh%p2,sz)
        p3=to_canvas(sh%p3,sz)

        write(unit_num, '(A)') '<polygon ' // attribute('points', &
               trim(adjustl(int_to_str(p1%x))) // ',' &
            // trim(adjustl(int_to_str(p1%y))) // ' ' &
            // trim(adjustl(int_to_str(p2%x))) // ',' &
            // trim(adjustl(int_to_str(p2%y))) // ' ' &
            // trim(adjustl(int_to_str(p3%x))) // ',' &
            // trim(adjustl(int_to_str(p3%y))),'') // ' ' &
            // attribute('fill', trim(adjustl(rgb_to_string(sh%fill_color))),'') &
            // attribute('stroke', trim(adjustl(rgb_to_string(sh%stroke_color))), '')&
            // '/>'
    end subroutine write_triangle

end module fig_svg_shapes
