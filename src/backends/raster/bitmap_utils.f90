module fig_bitmap_utils
    use fig_canvas
    use fig_rgb
    use plutovg
    use fig_config
    implicit none
    
contains
    
    subroutine set_rgba(pluto,color)
        type(c_ptr), intent(inout) :: pluto
        type(RGB) :: color
        call plutovg_set_rgba(pluto,normalize_ch(color%r),normalize_ch(color%g),normalize_ch(color%b),normalize_ch(color%a))
    end subroutine set_rgba

    function normalize_ch(ch) result(res)
        integer, intent(in) :: ch
        real(kind=8) :: res
        res= real(ch,kind=8)/real(2**rgb_bit_depth-1,kind=8)
    end function normalize_ch

    subroutine fill(pluto,color)
        type(c_ptr), intent(inout) :: pluto
        type(RGB) :: color
        if (color%a .ne. 0) then
            call set_rgba(pluto,color)
            call plutovg_fill_preserve(pluto)
        end if
    end subroutine fill

    subroutine stroke(pluto,color,width)
        type(c_ptr), intent(inout) :: pluto
        type(RGB) :: color
        real(kind=8) :: width
        if (color%a .ne. 0) then
            call set_rgba(pluto,color)
            call plutovg_set_line_width(pluto,width)
            call plutovg_stroke(pluto)
        else 
            call plutovg_new_path(pluto)
        end if

    end subroutine stroke



  
end module fig_bitmap_utils

