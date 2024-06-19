module fig_test
    implicit none
contains
    subroutine test_svg(canvas_name)
        character(len=*), intent(in) :: canvas_name
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
            call execute_command_line(trim(diff_command // ">"//diff_file))
            print *, "Svg Test failed."
            print *, "See differences in file: ", trim(diff_file)
        end if
    end subroutine test_svg
    subroutine test_both(canvas_name,current_canvas)
        character(len=*), intent(in) :: canvas_name
        type(bitmap_canvas), intent(inout) ::current_canvas
        call test_svg(canvas_name)


    end subroutine test_both

end module fig_test


