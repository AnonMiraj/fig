module fig_cairo_triangle
   use cairo
   use fig_shapes
   use fig_canvas
   use fig_cairo_utils

contains

   subroutine write_triangle(canva, cr, tri)
      class(base_canvas), intent(inout) :: canva
      type(c_ptr), intent(inout):: cr
      type(triangle), intent(in) :: tri
      type(canvas_point) :: p1, p2, p3

      p1 = to_canvas(tri%p1, canva%size)
      p2 = to_canvas(tri%p2, canva%size)
      p3 = to_canvas(tri%p3, canva%size)

      call cairo_move_to(cr, p1%x, p1%y)
      call cairo_line_to(cr, p2%x, p2%y)
      call cairo_line_to(cr, p3%x, p3%y)
      call cairo_line_to(cr, p1%x, p1%y)
      call cairo_close_path(cr)
      call fill(cr, tri)
      call stroke(cr, tri)

   end subroutine write_triangle

end module fig_cairo_triangle
