module fig_cairo_line
   use cairo
   use fig_shapes
   use fig_canvas
   use fig_cairo_utils

contains

   subroutine write_line(canva, cr, l)
      class(base_canvas), intent(inout) :: canva
      type(c_ptr), intent(inout):: cr
      type(line), intent(in) :: l
      type(canvas_point) :: p1, p2

      p1 = to_canvas(l%p1, canva%size)
      p2 = to_canvas(l%p2, canva%size)

      call cairo_move_to(cr, p1%x, p1%y)
      call cairo_line_to(cr, p2%x, p2%y)
      call cairo_close_path(cr)
      call stroke(cr, l)

   end subroutine write_line

end module fig_cairo_line
