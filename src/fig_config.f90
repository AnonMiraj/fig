module fig_config
    use iso_fortran_env, only: int32
    implicit none
    integer, parameter :: rgb_level = int32
    !! technically it should be int8 but there are problems with signed integers and bit manipulation
    integer, parameter :: pixel = int32 !! 8 bits per every color channel (r,g,b,a)
    integer :: rgb_bit_depth = 8
end module fig_config

