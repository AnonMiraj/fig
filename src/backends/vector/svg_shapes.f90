module fig_svg_shapes
    use fig_shapes
    use fig_svg_utils
contains

    subroutine write_circle(sh,unit_num)
        type(circle), intent(in) :: sh
        integer,intent(in) :: unit_num

        write(unit_num, '(A)') '<circle ' &
            // attribute('cx', trim(adjustl(real_to_str(sh%cx))), '') &
            // attribute('cy', trim(adjustl(real_to_str(sh%cy))), '') &
            // attribute('r', trim(adjustl(real_to_str(sh%r))), '') &
            // attribute('fill', trim(adjustl(rgb_to_string(sh%fill_color))), '') &
            // attribute('stroke', trim(adjustl(rgb_to_string(sh%stroke_color))), '') // '/>'
    end subroutine write_circle

    subroutine write_rectangle(sh,unit_num)
        type(rectangle), intent(in) :: sh
        integer,intent(in) :: unit_num

        write(unit_num, '(A)') '<rect ' &
            // attribute('x', trim(adjustl(real_to_str(sh%x))), '') &
            // attribute('y', trim(adjustl(real_to_str(sh%y))), '') &
            // attribute('width', trim(adjustl(real_to_str(sh%width))), '') &
            // attribute('height', trim(adjustl(real_to_str(sh%height))), '') &
            // attribute('fill', trim(adjustl(rgb_to_string(sh%fill_color))), '') // '/>'
    end subroutine write_rectangle

end module fig_svg_shapes
