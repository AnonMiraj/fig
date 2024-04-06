module fig_canvas
    implicit none
    
    type :: canvas
        integer :: width, height
        character(len=:), allocatable :: title
        integer, dimension(:,:), allocatable:: pixels
    end type canvas
    
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
        
        allocate(this%pixels(0:width, 0:height))
        this%pixels = 0
    end subroutine canvas_init

    subroutine fig_save_to_ppm_file(canva, result)
        implicit none
        type(canvas), intent(inout) :: canva
        character(len=:), allocatable :: file_path
        integer, intent(out) :: result
        integer :: iunit, i,j
        integer :: bytes(3)

        file_path=canva%title // '.ppm'
        open(newunit=iunit, file=file_path, status='replace')

        write(iunit, '(a2)') 'P6'
        write(iunit, '(i0," ",i0)')  canva%width, canva%height
        write(iunit, '(i0)') 255

        do j = 0, canva%height-1
            do i = 0, canva%width-1
                bytes(1) = ibits(canva%pixels(i, j),  0, 8)
                bytes(2) = ibits(canva%pixels(i, j),  8, 8)
                bytes(3) = ibits(canva%pixels(i, j), 16, 8)
        
                write(iunit, '(3a1)', advance='no') bytes
            end do
        end do

        close(iunit)
        result = 0
    end subroutine fig_save_to_ppm_file


end module fig_canvas

