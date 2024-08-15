module fig_config
    use iso_fortran_env, only: int32
    implicit none
    integer, parameter :: rgb_bit_depth = 8
    logical :: FIG_ABSOLUTE_COORDINATES = .false.
end module fig_config
