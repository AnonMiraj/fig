module cairo_extra
   use, intrinsic :: iso_c_binding
   implicit none
   interface

!! Warning These procedures do not check if x and y are in bounds or if the surface is initialized.

! int32_t get_pixel(const cairo_surface_t* surface, int x, int y);
      function get_pixel(surface, x, y) bind(c)
         import :: c_ptr, c_int, c_int32_t
         implicit none
         integer(c_int32_t) :: get_pixel
         type(c_ptr), value :: surface
         integer(c_int), value :: x
         integer(c_int), value :: y
      end function

! void set_pixel(const cairo_surface_t* surface, int x, int y,int32_t pixel);
      subroutine set_pixel(surface, x, y, pixel) bind(c)
         import :: c_ptr, c_int, c_int32_t
         implicit none
         type(c_ptr), value :: surface
         integer(c_int), value :: x
         integer(c_int), value :: y
         integer(c_int32_t), value :: pixel
      end subroutine

   end interface
end module cairo_extra
