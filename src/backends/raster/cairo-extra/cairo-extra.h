#include <cairo/cairo.h>
#include <stdint.h>
int32_t get_pixel(cairo_surface_t* surface, int x, int y) ;
void set_pixel(cairo_surface_t* surface, int x, int y, uint32_t pixel) ;
