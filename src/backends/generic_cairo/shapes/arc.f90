module fig_cairo_arc
   use cairo
   use fig_shapes
   use fig_canvas
   use fig_cairo_utils

contains

   subroutine write_arc(canva, cr, sh)
      class(base_canvas), intent(inout) :: canva
      type(c_ptr), intent(inout):: cr
      type(arc), intent(in) :: sh
      type(canvas_point) :: c

      c = to_canvas(sh%c, canva%size)
      call cairo_arc(cr, c%x, c%y, sh%r, sh%start_angle, sh%end_angle)
      call fill(cr, sh)
      call stroke(cr, sh)

   end subroutine write_arc

end module fig_cairo_arc

