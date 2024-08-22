module fig_shapes
   use fig_types
   use fig_rgb_color_constants
   use fig_rgb

   type, abstract :: shape
      type(RGB) :: fill_color = FIG_COLOR_BLANK
      type(RGB) :: stroke_color = FIG_COLOR_BLANK
      real(kind=8) :: stroke_width = 1
      real(kind=8) :: dash_offset = 0
      real(kind=8), allocatable :: dash_array(:)
   end type shape

   type, extends(shape) :: arc
      type(point) :: c
      real(kind=8):: r
      real(kind=8):: start_angle
      real(kind=8):: end_angle
   end type arc

   type, extends(shape) :: circle
      type(point) :: c
      real(kind=8):: r
   end type circle

   type, extends(shape) :: ellipse
      type(point) :: c
      real(kind=8):: rx, ry
   end type ellipse

   type, extends(shape) :: rectangle
      type(point) :: p !this means the upper left point of the rectangle
      real(kind=8):: width, height
      real(kind=8):: rx = 0, ry = 0
      !! todo make it so if only one of them has a value make it a circular corner
   end type rectangle

   type, extends(shape) :: triangle
      type(point) :: p1, p2, p3
   end type triangle

   type, extends(shape) :: line
      type(point) :: p1, p2
   end type line

   integer, parameter :: FIG_FONT_SLANT_NORMAL = 0
   integer, parameter :: FIG_FONT_SLANT_ITALIC = 1
   integer, parameter :: FIG_FONT_SLANT_OBLIQUE = 2

   integer, parameter :: FIG_FONT_WEIGHT_NORMAL = 0
   integer, parameter :: FIG_FONT_WEIGHT_BOLD = 1

   type, extends(shape) :: text
      type(point) :: p
      character(len=:), allocatable ::content
      character(len=:), allocatable ::font_family
      integer :: slant = FIG_FONT_SLANT_NORMAL
      integer :: weight = FIG_FONT_WEIGHT_NORMAL
      real(kind=8):: size = 14
   end type text

   type, extends(shape) :: polyline
      character(len=:), allocatable :: poly_string
   contains
      procedure :: add_points
   end type polyline

   type, extends(polyline) :: polygon
   end type polygon

contains

   subroutine add_points(this, x, y, n)
      class(polyline), intent(inout) :: this
      real, intent(in) :: x(:)
      real, intent(in) :: y(:)
      integer, intent(in) :: n
      character(len=20) :: point_string
      integer :: i
      character(len=:), allocatable :: temp_string

      temp_string = ""

      do i = 1, n
         write (point_string, '(F6.2,",",F6.2)') x(i), y(i)
         if (i == 1) then
            temp_string = trim(point_string)
         else
            temp_string = trim(temp_string)//" "//trim(point_string)
         end if
      end do

      this%poly_string = temp_string
   end subroutine add_points

end module fig_shapes
