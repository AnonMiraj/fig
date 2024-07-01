module fig_test
    use plutovg
    use plutovg_extra
    use fig_bitmap
    use fig_config
    use fig_rgb_color_constants
    use fig_rgb
    implicit none

    
contains
    subroutine test_svg(canvas_name,err)
        character(len=*), intent(in) :: canvas_name
        integer,intent(out) :: err
        integer :: status
        character(len=256) :: current_file, expected_file, diff_command, diff_file
        character(:),allocatable :: diff_output
        current_file = canvas_name // ".svg"
        expected_file = "test/expected/" // canvas_name // ".svg"
        diff_file = canvas_name // "_svg.diff"
        diff_command ="diff -u " // trim(current_file) // " " // trim(expected_file)
        call execute_command_line(trim(diff_command//" > /dev/null"), exitstat=status)

        if (status == 0) then
            print *, canvas_name," Svg test passed."
        else
            call execute_command_line(trim(diff_command)// ">" //diff_file)
            print *, "!!Svg Test failed."//"See differences in file: "//trim(diff_file)
            err = 1 
        end if
    end subroutine test_svg

    subroutine test_bitmap(canvas_name,current_canvas,err)
        character(len=*), intent(in) :: canvas_name
        type(bitmap_canvas), intent(inout) ::current_canvas
        integer,intent(out) :: err
        type(bitmap_canvas)::expected_canvas
        type(bitmap_canvas)::diff_canvas
        integer :: status
        logical :: failed = .false.
        character(len=256) :: current_file, expected_file, diff_command, diff_file
        character(:),allocatable :: diff_output
        integer :: i , j
        integer(c_int32_t) :: diff_color , current_pixel, expected_pixel

        current_file = canvas_name // ".ppm"
        expected_file = "test/expected/" // canvas_name // ".ppm"
        diff_file = canvas_name // ".diff"
        diff_color = rgb_to_int(FIG_COLOR_RED)

        call expected_canvas%load_from_ppm(expected_file)
        call diff_canvas%init(expected_canvas%size%width,expected_canvas%size%height)


        do j = 0, expected_canvas%size%height - 1
            do i = 0,expected_canvas%size%width - 1
                call set_pixel(diff_canvas%surface,i,j,diff_color)
            end do
        end do

        if (expected_canvas%size%width/=current_canvas%size%width&
            .or. expected_canvas%size%height/=current_canvas%size%height )  then
            failed = .true.
        end if
        

        do j = 0, min(current_canvas%size%height,expected_canvas%size%height) - 1
            do i = 0,min(current_canvas%size%width,expected_canvas%size%width) - 1
                current_pixel=get_pixel(current_canvas%surface,i,j)
                expected_pixel=get_pixel(expected_canvas%surface,i,j)
                if (current_pixel==expected_pixel) then
                    call set_pixel(diff_canvas%surface,i,j,current_pixel)
                else 
                    failed = .true.
                    call set_pixel(diff_canvas%surface,i,j,diff_color)
                end if
            end do
        end do

         if (failed) then
             call diff_canvas%save_to_ppm(diff_file)
             print *, "!!bitmap test failed." // "See differences in file: "// trim(diff_file)//".ppm"
             err = 1
         else
             print *, canvas_name," bitmap test passed."
         end if
    end subroutine test_bitmap

    subroutine test_both(canvas_name,current_canvas)
        character(len=*), intent(in) :: canvas_name
        type(bitmap_canvas), intent(inout) ::current_canvas
        integer :: svg_err,bitmap_err
        call test_svg(canvas_name,svg_err)
        !!call test_bitmap(canvas_name,current_canvas,bitmap_err) 
        !! TODO NEED fixing possibly use img_diff instead
        if (svg_err==1 .or. bitmap_err==1 ) error stop


    end subroutine test_both

end module fig_test


