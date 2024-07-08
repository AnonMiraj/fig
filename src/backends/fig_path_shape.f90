module fig_path
    use fig_shapes
    implicit none

    type, extends(shape) :: path
        character(len=:), allocatable :: path_string
    contains
        procedure :: moveTo
        procedure :: lineTo
        procedure :: bezierCurveTo
        procedure :: quadraticCurveTo
        procedure :: ellipticalArcTo
        procedure :: closePath
    end type path

contains

    subroutine moveTo(this, x, y)
        class(path), intent(inout) :: this
        real, intent(in) :: x, y
        character(len=30) :: command

        write(command, '(A,F6.2,A,F6.2,A)') "M ", x, " ", y
        this%path_string = trim(this%path_string) // trim(command)
    end subroutine moveTo

    subroutine lineTo(this, x, y)
        class(path), intent(inout) :: this
        real, intent(in) :: x, y
        character(len=30) :: command

        write(command, '(A,F6.2,A,F6.2,A)') "L ", x, " ", y
        this%path_string = trim(this%path_string) // trim(command)
    end subroutine lineTo

    subroutine bezierCurveTo(this, x1, y1, x2, y2, x, y)
        class(path), intent(inout) :: this
        real, intent(in) :: x1, y1, x2, y2, x, y
        character(len=50) :: command

        write(command, '(A,F6.2,A,F6.2,A,F6.2,A,F6.2,A,F6.2,A,F6.2,A)') "C ", x1, " ", y1, " ", x2, " ", y2, " ", x, " ", y

        this%path_string = trim(this%path_string) // trim(command)
    end subroutine bezierCurveTo

    subroutine quadraticCurveTo(this, x1, y1, x, y)
        class(path), intent(inout) :: this
        real, intent(in) :: x1, y1, x, y
        character(len=50):: command

        write(command, '(A,F6.2,A,F6.2,A,F6.2,A,F6.2,A)') "Q ", x1, " ", y1, " ", x, " ", y, " " 
        this%path_string = trim(this%path_string) // trim(command)
    end subroutine quadraticCurveTo

    subroutine ellipticalArcTo(this, rx, ry, x_axis_rotation, large_arc_flag, sweep_flag, x, y)
        class(path), intent(inout) :: this
        real, intent(in) :: rx, ry, x_axis_rotation, x, y
        INTEGER, intent(in) :: large_arc_flag, sweep_flag
        character(len=80) :: command

        write(command, '(A,F6.2,A,F6.2,A,F6.2,A,I0,A,I0,A,F6.2,A,F6.2,A)') &
            "A ", rx, " ", ry, " ", x_axis_rotation, " ", large_arc_flag, " ", &
            sweep_flag, " ", x, " ", y
        this%path_string = trim(this%path_string) // trim(command)
    end subroutine ellipticalArcTo

    subroutine closePath(this)
        class(path), intent(inout) :: this

        this%path_string = trim(this%path_string) // "Z "
    end subroutine closePath

end module fig_path
