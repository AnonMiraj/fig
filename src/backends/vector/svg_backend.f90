module fig_svg
   use cairo
   use cairo_enums
   use cairo_types
   use cairo_extra
   use fig_drawing
   use fig_cairo
   implicit none
   type, extends(cairo_canvas) :: svg_canvas
   contains
      procedure :: init => init_svg
      procedure :: save_to_svg
   end type svg_canvas
contains

   subroutine init_svg(this, width, height, title)
      class(svg_canvas), intent(inout) :: this
      integer, intent(in) :: width, height
      character(len=*), intent(in) :: title

      this%size%width = width
      this%size%height = height
      this%title = title

      this%surface = cairo_svg_surface_create(title//".svg"//c_null_char, real(width, kind=8), real(height, kind=8))
      this%cairo = cairo_create(this%surface)
   end subroutine init_svg

   subroutine save_to_svg(this)
      class(svg_canvas), intent(inout) :: this

      call cairo_surface_finish(this%surface); 
   end subroutine save_to_svg

end module fig_svg

