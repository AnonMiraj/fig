module fig_cairo_poly
   use cairo
   use fig_shapes
   use fig_canvas
   use fig_cairo_utils

   private :: parse_string
contains

   subroutine write_polyline(canva, cr, pl)
      class(base_canvas), intent(inout) :: canva
      type(c_ptr), intent(inout):: cr
      type(polyline), intent(in) :: pl
      type(point), allocatable:: points(:)
      integer :: p_count, i
      type(canvas_point) :: p
      call parse_string(pl%poly_string, points, p_count)

      p = to_canvas(points(1), canva%size)
      call cairo_move_to(cr, p%x, p%y)
      do i = 2, p_count, 1
         p = to_canvas(points(i), canva%size)
         call cairo_line_to(cr, p%x, p%y)
      end do

      call fill(cr, pl)
      call stroke(cr, pl)
   end subroutine write_polyline

   subroutine write_polygon(canva, cr, pl)
      class(base_canvas), intent(inout) :: canva
      type(c_ptr), intent(inout):: cr
      type(polygon), intent(in) :: pl
      type(point), allocatable:: points(:)
      integer :: p_count, i
      type(canvas_point) :: p
      call parse_string(pl%poly_string, points, p_count)

      p = to_canvas(points(1), canva%size)
      call cairo_move_to(cr, p%x, p%y)
      do i = 2, p_count, 1
         p = to_canvas(points(i), canva%size)
         call cairo_line_to(cr, p%x, p%y)
      end do
      p = to_canvas(points(1), canva%size)
      call cairo_line_to(cr, p%x, p%y)

      call fill(cr, pl)
      call stroke(cr, pl)
   end subroutine write_polygon

   subroutine parse_string(input_str, points, p_count)
      implicit none
      character(len=*), intent(in) :: input_str
      type(point), allocatable, intent(out) :: points(:)
      integer, intent(out) :: p_count
      integer :: i, length
      character(len=1) :: current_char
      character(len=20) :: temp_string
      logical :: is_num

      p_count = -1
      length = len_trim(input_str)

      allocate (points(length))

      i = 1
      do while (i <= length)
         current_char = input_str(i:i)

         if (current_char == '-') then
            temp_string = '-'
            i = i + 1
            if (i <= length) current_char = input_str(i:i)
            is_num = .true.
            do while (is_num .and. i <= length)
               if ((current_char >= '0' .and. current_char <= '9') .or. current_char == '.') then
                  temp_string = trim(temp_string)//current_char
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
            if (len_trim(temp_string) > 0 .and. temp_string /= '-') then
               p_count = p_count + 1

               if (iand(p_count, 1) /= 1) then
                  read (temp_string, *) points(p_count/2 + 1)%x
               else
                  read (temp_string, *) points(p_count/2 + 1)%y
               end if
            end if
         else if ((current_char >= '0' .and. current_char <= '9') .or. current_char == '.') then
            temp_string = ''
            is_num = .true.
            do while (is_num .and. i <= length)
               if ((current_char >= '0' .and. current_char <= '9') .or. current_char == '.') then
                  temp_string = trim(temp_string)//current_char
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
               p_count = p_count + 1

               if (iand(p_count, 1) /= 1) then
                  read (temp_string, *) points(p_count/2 + 1)%x
               else
                  read (temp_string, *) points(p_count/2 + 1)%y
               end if
            end if
         else
            i = i + 1
         end if
      end do
      p_count = (p_count + 1)/2

   end subroutine parse_string
end module fig_cairo_poly
