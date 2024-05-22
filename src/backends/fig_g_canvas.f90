module fig_g_canvas
    use fig_svg
    use fig_bitmap
    use fig_canvas
    implicit none

    type,extends(base_canvas) :: g_canvas !! will make it canvas after full migration
    contains
        procedure :: save_to_file 
    end type g_canvas

contains
   subroutine save_to_file(this,output_format)
        class(g_canvas), intent(inout) :: this
        character(len=*), intent(in) :: output_format
        type(bitmap_canvas):: temp_canvas

        select case (trim(output_format))
        case ('svg')
            call save_to_svg(this,this%shapes)
        case default

            call temp_canvas%init(this%width,this%height,this%title)
            call temp_canvas%apply_shapes(this%shapes)
            call temp_canvas%save_to_ppm()
        end select
    end subroutine save_to_file


end module fig_g_canvas
