module fig_cairo_rect
   use cairo
   use fig_shapes
   use fig_canvas
   use fig_cairo_utils

contains

   subroutine write_rectangle(canva, cr, rect)
      class(base_canvas), intent(inout) :: canva
      type(c_ptr), intent(inout):: cr
      type(rectangle), intent(in) :: rect
      type(canvas_point) :: p
      real(kind=8) :: right, bottom
      real(kind=8) :: rx, ry, cpx, cpy

      p = to_canvas(rect%p, canva%size)

      if (rect%rx < 1 .or. rect%ry < 1) then
         call cairo_rectangle(cr, p%x, p%y, rect%width, rect%height)
      else
         rx = min(rect%rx, rect%width*0.5)
         ry = min(rect%ry, rect%height*0.5)

         right = p%x + rect%width
         bottom = p%y + rect%height
         cpx = rx*kappa
         cpy = ry*kappa

         call cairo_move_to(cr, p%x, p%y + ry); 
         call cairo_curve_to(cr, p%x, p%y + ry - cpy, p%x + rx - cpx, p%y, p%x + rx, p%y); 
         call cairo_line_to(cr, right - rx, p%y); 
         call cairo_curve_to(cr, right - rx + cpx, p%y, right, p%y + ry - cpy, right, p%y + ry); 
         call cairo_line_to(cr, right, bottom - ry); 
         call cairo_curve_to(cr, right, bottom - ry + cpy, right - rx + cpx, bottom, right - rx, bottom); 
         call cairo_line_to(cr, p%x + rx, bottom); 
         call cairo_curve_to(cr, p%x + rx - cpx, bottom, p%x, bottom - ry + cpy, p%x, bottom - ry); 
         call cairo_line_to(cr, p%x, p%y + ry); 
         call cairo_close_path(cr); 
      end if

      call fill(cr, rect)
      call stroke(cr, rect)
   end subroutine write_rectangle

end module fig_cairo_rect
