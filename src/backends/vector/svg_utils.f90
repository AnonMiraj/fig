module fig_svg_utils
    implicit none
contains

    function real_to_str(value) result(str)
        real(kind=8), intent(in) :: value
        character(len=100) :: str
        write(str, '(F10.2)') value
        return
    end function real_to_str
    
    function int_to_str(value) result(str)
        integer, intent(in) :: value
        character(len=100) :: str
        write(str, '(I0)') value
    end function int_to_str

    function attribute(attribute_name, value, unit) result(attribute_str)
        character(len=*), intent(in) :: attribute_name, value, unit
        character(len=:), allocatable :: attribute_str
        attribute_str = trim(attribute_name) // '="' // trim(value) // trim(unit) // '" '
        return
    end function attribute

end module fig_svg_utils

