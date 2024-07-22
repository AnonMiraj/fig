module fig_poly
    use fig_shapes
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
            write(point_string, '(F6.2,",",F6.2)') x(i), y(i)
            if (i == 1) then
                temp_string = trim(point_string)
            else
                temp_string = trim(temp_string) // " " // trim(point_string)
            end if
        end do

        this%poly_string = temp_string
    end subroutine add_points
end module fig_poly
