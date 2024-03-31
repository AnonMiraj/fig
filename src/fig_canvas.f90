module fig_canvas
    implicit none
    
    type :: canvas
        integer :: width, height
        character(len=:), allocatable :: title
        integer, dimension(:,:), allocatable:: pixels
    end type canvas
    

contains


    subroutine canvas_init(this, width, height, title)
        type(canvas), intent(out) :: this
        integer, intent(in) :: width, height
        character(len=*), intent(in) :: title
        
        this%width = width
        this%height = height
        this%title = title
        
        allocate(this%pixels(0:width-1, 0:height-1))
        this%pixels = 0
    end subroutine canvas_init

end module fig_canvas

