module fig_cairo_utils
   use fig_canvas
   use fig_rgb
   use cairo
   use fig_config
   implicit none
   real(kind=8), parameter :: kappa = 0.55228474983079339840

contains

   subroutine set_rgba(cr, color)
      type(c_ptr), intent(inout) :: cr
      type(RGB) :: color
      call cairo_set_source_rgba(cr, (color%r), (color%g), (color%b), (color%a))

   end subroutine set_rgba

   subroutine fill(cr, sh)
      type(c_ptr), intent(inout) :: cr
      class(shape), intent(in) :: sh
      if (sh%fill_color%a > 0.0001) then
         call set_rgba(cr, sh%fill_color)
         call cairo_fill_preserve(cr)
      end if
   end subroutine fill

   subroutine stroke(cr, sh)
      type(c_ptr), intent(inout) :: cr
      class(shape), intent(in) :: sh
      real(kind=8), dimension(:), allocatable, target :: dash_arr
      if (allocated(sh%dash_array)) then
         allocate (dash_arr(size(sh%dash_array)))
         dash_arr = sh%dash_array
         call cairo_set_dash(cr, c_loc(dash_arr), size(sh%dash_array), sh%dash_offset)
         deallocate (dash_arr)
      end if

      if (sh%stroke_color%a > 0.0001) then
         call set_rgba(cr, sh%stroke_color)
         call cairo_set_line_width(cr, sh%stroke_width)
         call cairo_stroke(cr)
      else
         call cairo_new_path(cr)
      end if

      if (allocated(sh%dash_array)) then
         call cairo_set_dash(cr, c_null_ptr, 0, real(0, kind=8))
      end if
   end subroutine stroke

   subroutine quad_to(cr, cx, cy, x1, y1, x2, y2)
      type(c_ptr), intent(inout) :: cr
      real(kind=8), intent(in) :: cx, cy, x1, y1, x2, y2
      real(kind=8):: cx1, cy1
      real(kind=8):: cx2, cy2

      cx1 = 2.0/3.0*x1 + 1.0/3.0*cx
      cy1 = 2.0/3.0*y1 + 1.0/3.0*cy
      cx2 = 2.0/3.0*x1 + 1.0/3.0*x2
      cy2 = 2.0/3.0*y1 + 1.0/3.0*y2
      call cairo_curve_to(cr, cx1, cy1, cx2, cy2, x2, y2)

   end subroutine quad_to

   subroutine arc_to(cr, cx, cy, rx, ry, x_axis_rotation, large_arc_flag, sweep_flag, x, y)
      type(c_ptr), intent(inout) :: cr
      real(kind=8), intent(in) :: cx, cy, rx, ry, x_axis_rotation, x, y
      logical, intent(in) :: large_arc_flag, sweep_flag
      real(kind=8) :: pi, sin_th, cos_th, dx, dy, dx1, dy1, Pr1, Pr2
      real(kind=8) :: Px, Py, check, a00, a01, a10, a11, x0, y0, x1, y1, d
      real(kind=8) :: sfactor_sq, sfactor, xc, yc, th0, th1, th_arc, th2, th3
      real(kind=8) :: a00_, a01_, a10_, a11_, thHalf, t, x1_, y1_, x3_, y3_, x2_, y2_
      real(kind=8) :: rx_, ry_
      real(kind=8) :: cx1, cx2, cx3
      real(kind=8) :: cy1, cy2, cy3
      integer :: n_segs, i

      pi = atan(1.0)*4.0

      rx_ = abs(rx)
      ry_ = abs(ry)

      sin_th = sin(x_axis_rotation*pi/180.0)
      cos_th = cos(x_axis_rotation*pi/180.0)

      dx = (cx - x)/2.0
      dy = (cy - y)/2.0
      dx1 = cos_th*dx + sin_th*dy
      dy1 = -sin_th*dx + cos_th*dy
      Pr1 = rx_*rx_
      Pr2 = ry_*ry_
      Px = dx1*dx1
      Py = dy1*dy1
      check = Px/Pr1 + Py/Pr2

      if (check > 1.0) then
         rx_ = rx_*sqrt(check)
         ry_ = ry_*sqrt(check)
      end if

      a00 = cos_th/rx_
      a01 = sin_th/rx_
      a10 = -sin_th/ry_
      a11 = cos_th/ry_
      x0 = a00*cx + a01*cy
      y0 = a10*cx + a11*cy
      x1 = a00*x + a01*y
      y1 = a10*x + a11*y
      d = (x1 - x0)*(x1 - x0) + (y1 - y0)*(y1 - y0)
      sfactor_sq = 1.0/d - 0.25
      if (sfactor_sq < 0.0) sfactor_sq = 0.0
      sfactor = sqrt(sfactor_sq)
      if (sweep_flag .eqv. large_arc_flag) sfactor = -sfactor
      xc = 0.5*(x0 + x1) - sfactor*(y1 - y0)
      yc = 0.5*(y0 + y1) + sfactor*(x1 - x0)

      th0 = atan2(y0 - yc, x0 - xc)
      th1 = atan2(y1 - yc, x1 - xc)

      th_arc = th1 - th0
      if (th_arc < 0.0 .and. sweep_flag) then
         th_arc = th_arc + 2.0*pi
      else if (th_arc > 0.0 .and. .not. sweep_flag) then
         th_arc = th_arc - 2.0*pi
      end if

      n_segs = int(ceiling(abs(th_arc/(pi*0.5 + 0.001))))
      do i = 1, n_segs
         th2 = th0 + (i - 1)*th_arc/n_segs
         th3 = th0 + i*th_arc/n_segs

         a00_ = cos_th*rx_
         a01_ = -sin_th*ry_
         a10_ = sin_th*rx_
         a11_ = cos_th*ry_

         thHalf = 0.5*(th3 - th2)
         t = (8.0/3.0)*sin(thHalf*0.5)**2/sin(thHalf)
         x1_ = xc + cos(th2) - t*sin(th2)
         y1_ = yc + sin(th2) + t*cos(th2)
         x3_ = xc + cos(th3)
         y3_ = yc + sin(th3)
         x2_ = x3_ + t*sin(th3)
         y2_ = y3_ - t*cos(th3)

         cx1 = a00_*x1_ + a01_*y1_
         cy1 = a10_*x1_ + a11_*y1_
         cx2 = a00_*x2_ + a01_*y2_
         cy2 = a10_*x2_ + a11_*y2_
         cx3 = a00_*x3_ + a01_*y3_
         cy3 = a10_*x3_ + a11_*y3_

         call cairo_curve_to(cr, cx1, cy1, cx2, cy2, cx3, cy3)
      end do

   end subroutine arc_to

end module fig_cairo_utils
