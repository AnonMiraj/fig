module fig_cairo_ellipse
   use cairo
   use fig_shapes
   use fig_canvas
   use fig_cairo_utils

contains

   subroutine write_ellipse(canva, cr, ellip)
      class(base_canvas), intent(inout) :: canva
      type(c_ptr), intent(inout):: cr
      type(ellipse), intent(in) :: ellip
      type(canvas_point) :: c
      real(kind=8) :: left, top, right, bottom, cpx, cpy

      c = to_canvas(ellip%c, canva%size)

      left = c%x - ellip%rx; 
      top = c%y - ellip%ry; 
      right = c%x + ellip%rx; 
      bottom = c%y + ellip%ry; 
      cpx = ellip%rx*kappa; 
      cpy = ellip%ry*kappa; 
      call cairo_move_to(cr, c%x, top)
      call cairo_curve_to(cr, c%x + cpx, top, right, c%y - cpy, right, c%y); 
      call cairo_curve_to(cr, right, c%y + cpy, c%x + cpx, bottom, c%x, bottom); 
      call cairo_curve_to(cr, c%x - cpx, bottom, left, c%y + cpy, left, c%y); 
      call cairo_curve_to(cr, left, c%y - cpy, c%x - cpx, top, c%x, top); 
      call cairo_close_path(cr); 
      call fill(cr, ellip)
      call stroke(cr, ellip)

   end subroutine write_ellipse

end module fig_cairo_ellipse
