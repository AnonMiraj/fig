module fig_canvas
    use fig_config
    use fig_shapes
    implicit none
    
    type :: canvas
        integer :: width, height
        character(len=:), allocatable :: title
        integer(pixel), dimension(:,:), allocatable:: pixels
    end type canvas
    
    type, abstract :: base_canvas 
        real :: width, height
    contains
        procedure(canvas_draw_shape), deferred :: draw_shape
        procedure :: init
    end type base_canvas

    abstract interface
       subroutine canvas_draw_shape(canva,sh)
       import base_canvas, shape 
       class(base_canvas), intent(inout) :: canva
       class(shape), intent(in) :: sh
       end subroutine canvas_draw_shape
    end interface

    type :: vec2
        integer :: x, y
    end type vec2

    type :: vec3
        integer :: x, y, z
    end type vec3

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


    subroutine init(this, width, height)
        class(base_canvas), intent(inout) :: this
        real, intent(in) :: width, height

        this%width = width
        this%height = height

    end subroutine init



    subroutine fig_save_to_ppm_file(canva, result,optional_file_path)
        implicit none
        type(canvas), intent(inout) :: canva
        character(len=:), allocatable, optional :: optional_file_path
        character(len=:), allocatable :: file_path
        integer, intent(out) :: result
        integer :: iunit, i,j
        integer :: bytes(3)

        file_path=canva%title // '.ppm'

        if(present(optional_file_path)) then
            file_path=optional_file_path
        end if

        open(newunit=iunit, file=file_path, status='replace')

        write(iunit, '(a2)') 'P6'
        write(iunit, '(i0," ",i0)')  canva%width, canva%height
        write(iunit, '(i0)') 2**rgb_bit_depth-1
        do j = 0, canva%height-1
            do i = 0, canva%width-1
                bytes(1) = ibits(canva%pixels(i, j), 0, rgb_bit_depth)
                bytes(2) = ibits(canva%pixels(i, j), rgb_bit_depth, rgb_bit_depth)
                bytes(3) = ibits(canva%pixels(i, j), 2*rgb_bit_depth, rgb_bit_depth)
        
                write(iunit, '(3a1)', advance='no') bytes
            end do
        end do

        close(iunit)
        result = 0
    end subroutine fig_save_to_ppm_file


end module fig_canvas
