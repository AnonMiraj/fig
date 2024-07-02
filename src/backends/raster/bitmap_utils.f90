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

    subroutine fill(cr,color)
        type(c_ptr), intent(inout) :: cr
        type(RGB) :: color
        if (color%a .ne. 0) then
            call set_rgba(cr,color)
            call cairo_fill_preserve(cr)
        end if
    end subroutine fill

    subroutine stroke(cr,color,width)
        type(c_ptr), intent(inout) :: cr
        type(RGB) :: color
        real(kind=8) :: width
        if (color%a .ne. 0) then
            call set_rgba(cr,color)
            call cairo_set_line_width(cr,width)
            call cairo_stroke(cr)
        else 
            call cairo_new_path(cr)
        end if

    end subroutine stroke

  
end module fig_bitmap_utils

