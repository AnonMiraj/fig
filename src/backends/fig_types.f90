module fig_types
   use fig_config
   implicit none

   type :: point
      real(kind=8) :: x
      real(kind=8) :: y
   end type point

   type :: canvas_point
      real(kind=8) :: x
      real(kind=8) :: y
   end type canvas_point

   interface operator(+)
      module procedure canvas_point_add
   end interface

   type :: canvas_size
      integer :: width
      integer :: height
   end type canvas_size

contains

   elemental type(canvas_point) function to_canvas(p, sz) result(pxl)
      type(point), intent(in) :: p
      type(canvas_size), intent(in) :: sz

      if (FIG_ABSOLUTE_COORDINATES) then
         pxl%x = p%x
         pxl%y = p%y
      else
         pxl%x = p%x*sz%width
         pxl%y = p%y*sz%height
      end if
   end function to_canvas

   function canvas_point_add(p1, p2) result(result_point)
      type(canvas_point), intent(in) :: p1, p2
      type(canvas_point) :: result_point

      result_point%x = p1%x + p2%x
      result_point%y = p1%y + p2%y
   end function canvas_point_add
end module fig_types
