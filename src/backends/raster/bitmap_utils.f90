module fig_bitmap_utils
    use fig_canvas
    use fig_rgb
    use cairo
    use fig_config
    implicit none
    
contains
    
    subroutine set_rgba(cr,color)
        type(c_ptr), intent(inout) :: cr
        type(RGB) :: color
         call cairo_set_source_rgba(cr,normalize_ch(color%r),normalize_ch(color%g),normalize_ch(color%b),normalize_ch(color%a))
    end subroutine set_rgba

    function normalize_ch(ch) result(res)
        integer, intent(in) :: ch
        real(kind=8) :: res
        res= real(ch,kind=8)/real(2**rgb_bit_depth-1,kind=8)
    end function normalize_ch

    subroutine grade(cr,grad)
        type(c_ptr), intent(inout) :: cr
        type(linear_gradient) :: grad
        integer :: i
        type(c_ptr):: gg

        gg= cairo_pattern_create_linear(grad%x1,grad%y1,grad%x2,grad%y2)
        do i = 1, grad%stops%stop_count
           call cairo_pattern_add_color_stop_rgba(gg, &
            grad%stops%stop_array(i)%offset, &
            normalize_ch(grad%stops%stop_array(i)%stop_color%r), &
            normalize_ch(grad%stops%stop_array(i)%stop_color%g), &
            normalize_ch(grad%stops%stop_array(i)%stop_color%b), &
            normalize_ch(grad%stops%stop_array(i)%stop_color%a))
        end do
        call cairo_set_source(cr,gg)

    end subroutine grade


    subroutine fill(cr,sh)
        type(c_ptr), intent(inout) :: cr
        class(shape), intent(in) :: sh

        select type (color =>sh%fill_color2%pat)
            type is (RGB)
                if (color%a .ne. 0) then
                    call set_rgba(cr,sh%fill_color)
                    call cairo_fill_preserve(cr)
                end if               
            type is (linear_gradient)

                if (color%x2.ne.0) then
                    call grade(cr,color)
                    call cairo_fill_preserve(cr)
                end if
        end select

    end subroutine fill

    subroutine stroke(cr,sh)
        type(c_ptr), intent(inout) :: cr
        class(shape), intent(in) :: sh
        if (sh%stroke_color%a .ne. 0) then
            call set_rgba(cr,sh%stroke_color)
            call cairo_set_line_width(cr,sh%stroke_width)
            call cairo_stroke(cr)
        else 
            call cairo_new_path(cr)
        end if

    end subroutine stroke

  
end module fig_bitmap_utils

