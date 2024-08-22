module fig_path
   use fig_shapes
   implicit none

   type, extends(shape) :: path
      character(len=:), allocatable :: path_string
   contains
      procedure :: move_to
      procedure :: line_to
      procedure :: bezier_curve_to
      procedure :: quadratic_curve_to
      procedure :: elliptical_arc_to
      procedure :: close_path
   end type path

contains

   subroutine move_to(this, x, y)
      class(path), intent(inout) :: this
      real, intent(in) :: x, y
      character(len=30) :: command

      write (command, '(A,F6.2,A,F6.2,A)') "M ", x, " ", y
      this%path_string = trim(this%path_string)//trim(command)
   end subroutine move_to

   subroutine line_to(this, x, y)
      class(path), intent(inout) :: this
      real, intent(in) :: x, y
      character(len=30) :: command

      write (command, '(A,F6.2,A,F6.2,A)') "L ", x, " ", y
      this%path_string = trim(this%path_string)//trim(command)
   end subroutine line_to

   subroutine bezier_curve_to(this, x1, y1, x2, y2, x, y)
      ! x1, y1 - Coordinates of the first control point for the Bezier curve.
      ! x2, y2 - Coordinates of the second control point for the Bezier curve.
      ! x, y - Coordinates of the end point of the Bezier curve.
      class(path), intent(inout) :: this
      real, intent(in) :: x1, y1, x2, y2, x, y
      character(len=50) :: command

      write (command, '(A,F6.2,A,F6.2,A,F6.2,A,F6.2,A,F6.2,A,F6.2,A)') "C ", x1, " ", y1, " ", x2, " ", y2, " ", x, " ", y

      this%path_string = trim(this%path_string)//trim(command)
   end subroutine bezier_curve_to

   subroutine quadratic_curve_to(this, x1, y1, x, y)
      ! x1, y1 - Coordinates of the control point for the quadratic curve.
      ! x, y - Coordinates of the end point of the quadratic curve.
      class(path), intent(inout) :: this
      real, intent(in) :: x1, y1, x, y
      character(len=50):: command

      write (command, '(A,F6.2,A,F6.2,A,F6.2,A,F6.2,A)') "Q ", x1, " ", y1, " ", x, " ", y, " "
      this%path_string = trim(this%path_string)//trim(command)
   end subroutine quadratic_curve_to

   subroutine elliptical_arc_to(this, rx, ry, x_axis_rotation, large_arc_flag, sweep_flag, x, y)
      ! rx - Radius of the ellipse in the x direction.
      ! ry - Radius of the ellipse in the y direction.
      ! x_axis_rotation - Rotation of the ellipse in degrees.
      ! large_arc_flag - Flag indicating if the arc should be greater than 180 degrees (1 for true, 0 for false).
      ! sweep_flag - Flag indicating the direction of the arc (1 for clockwise, 0 for counterclockwise).
      ! x, y - Coordinates of the end point of the arc.
      class(path), intent(inout) :: this
      real, intent(in) :: rx, ry, x_axis_rotation, x, y
      integer, intent(in) :: large_arc_flag, sweep_flag
      character(len=80) :: command

      write (command, '(A,F6.2,A,F6.2,A,F6.2,A,I0,A,I0,A,F6.2,A,F6.2,A)') &
         "A ", rx, " ", ry, " ", x_axis_rotation, " ", large_arc_flag, " ", &
         sweep_flag, " ", x, " ", y
      this%path_string = trim(this%path_string)//trim(command)
   end subroutine elliptical_arc_to

   subroutine close_path(this)
      class(path), intent(inout) :: this

      this%path_string = trim(this%path_string)//"Z "
   end subroutine close_path

end module fig_path
