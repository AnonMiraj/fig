
module fig_cairo_path
    use cairo
    use fig_shapes
    use fig_canvas
    use fig_cairo_utils
    use fig_path

contains

    subroutine write_path(canva, cr, ph)
        class(base_canvas), intent(inout) :: canva
        type(c_ptr), intent(inout):: cr
        type(path), intent(in) :: ph
        character(len=1), allocatable:: char_array(:)
        real(kind=8), allocatable:: real_array(:)
        integer :: char_count, real_count,char_ind,real_ind
        type(canvas_point) :: cur_p,p1,p2,p3,p4,p5,p6
        type(point) :: temp_p
        real_ind =1
        
        cur_p%x=0
        cur_p%y=0

        call parse_string(ph%path_string, char_array, real_array, char_count, real_count)

        do char_ind = 1, char_count, 1

        print * , real_ind
        select case (char_array(char_ind))
            case ("M")
                temp_p%x=real_array(real_ind)
                temp_p%y=real_array(real_ind+1)
                cur_p= to_canvas(temp_p,canva%size)
                call cairo_move_to(cr,cur_p%x ,cur_p%y)
                real_ind = real_ind + 2
            case ("L")
                temp_p%x=real_array(real_ind)
                temp_p%y=real_array(real_ind+1)
                cur_p= to_canvas(temp_p,canva%size)
                call cairo_line_to(cr,cur_p%x ,cur_p%y)
                real_ind = real_ind + 2
            case ("C")

                temp_p%x=real_array(real_ind)
                temp_p%y=real_array(real_ind+1)
                p1= to_canvas(temp_p,canva%size)
                temp_p%x=real_array(real_ind+2)
                temp_p%y=real_array(real_ind+3)
                p2= to_canvas(temp_p,canva%size)
                temp_p%x=real_array(real_ind+4)
                temp_p%y=real_array(real_ind+5)
                cur_p= to_canvas(temp_p,canva%size)
                call cairo_curve_to(cr, p1%x, p1%y, p2%x, p2%y, cur_p%x, cur_p%y)
                real_ind = real_ind + 6
            case ("Q")
                temp_p%x=real_array(real_ind)
                temp_p%y=real_array(real_ind+1)
                p1= to_canvas(temp_p,canva%size)
                temp_p%x=real_array(real_ind+2)
                temp_p%y=real_array(real_ind+3)
                p2= to_canvas(temp_p,canva%size)
                call quad_to(cr, cur_p%x, cur_p%y, p1%x, p1%y, p2%x, p2%y)
                cur_p = p2
                real_ind = real_ind + 4
            case ("A")
                temp_p%x=real_array(real_ind)
                temp_p%y=real_array(real_ind+1)
                p1= to_canvas(temp_p,canva%size)
                temp_p%x=real_array(real_ind+5)
                temp_p%y=real_array(real_ind+6)
                p2= to_canvas(temp_p,canva%size)
                call arc_to(cr, cur_p%x, cur_p%y,p1%x, p1%y,&
                    real_array(real_ind+2), real_array(real_ind+3)>0.1, real_array(real_ind+4)>0.1, p2%x,&
                    p2%y)
                cur_p = p2
                real_ind = real_ind + 7
            case ("Z")
                real_ind = real_ind + 1
                call cairo_close_path(cr)
                
        end select

            
        end do



        call fill(cr,ph)
        call stroke(cr,ph)

       
    end subroutine write_path
    
    subroutine parse_string(input_str, char_array, real_array, char_count, real_count)
        implicit none
        character(len=*), intent(in) :: input_str
        character(len=1), allocatable, intent(out) :: char_array(:)
        real(kind=8), allocatable, intent(out) :: real_array(:)
        integer, intent(out) :: char_count, real_count
        integer :: i, length
        character(len=1) :: current_char
        character(len=20) :: temp_string
        logical :: is_num

        ! Initialize variables
        char_count = 0
        real_count = 0
        length = len_trim(input_str)

        allocate(char_array(length))  ! Allocate char_array based on input length
        allocate(real_array(length))  ! Allocate real_array based on input length

        i = 1
        do while (i <= length)
            current_char = input_str(i:i)

            if (current_char == '-') then
                ! Detecting a negative number
                temp_string = '-'
                i = i + 1
                current_char = input_str(i:i)
                is_num = .true.
                do while (is_num .and. i <= length)
                    if ((current_char >= '0' .and. current_char <= '9') .or. current_char == '.') then
                        temp_string = trim(temp_string) // current_char
                        i = i + 1
                        if (i <= length) then
                            current_char = input_str(i:i)
                        else
                            is_num = .false.
                        end if
                    else
                        is_num = .false.
                    end if
                end do
                if (len_trim(temp_string) > 0) then
                    real_count = real_count + 1
                    read(temp_string, *) real_array(real_count)
                end if
            else if ((current_char >= '0' .and. current_char <= '9') .or. current_char == '.') then
                ! Start of a non-negative number
                temp_string = ''
                is_num = .true.
                do while (is_num .and. i <= length)
                    if ((current_char >= '0' .and. current_char <= '9') .or. current_char == '.') then
                        temp_string = trim(temp_string) // current_char
                        i = i + 1
                        if (i <= length) then
                            current_char = input_str(i:i)
                        else
                            is_num = .false.
                        end if
                    else
                        is_num = .false.
                    end if
                end do
                if (len_trim(temp_string) > 0) then
                    real_count = real_count + 1
                    read(temp_string, *) real_array(real_count)
                end if
            else if ((current_char >= 'A' .and. current_char <= 'Z') .or. (current_char >= 'a' .and. current_char <= 'z')) then
                char_count = char_count + 1
                char_array(char_count) = current_char
                i = i + 1
            else
                i = i + 1
            end if
        end do

    end subroutine parse_string
end module fig_cairo_path
