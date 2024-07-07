#include "cairo-extra.h"

int32_t get_pixel(cairo_surface_t* surface, int x, int y) {
    cairo_format_t format = cairo_image_surface_get_format(surface);

    unsigned char* data = cairo_image_surface_get_data(surface);
    int stride = cairo_image_surface_get_stride(surface);

    uint32_t* p = (uint32_t*)(data + stride * y);

    return p[x];
}

void set_pixel(cairo_surface_t* surface, int x, int y, uint32_t pixel) {
    cairo_format_t format = cairo_image_surface_get_format(surface);

    unsigned char* data = cairo_image_surface_get_data(surface);
    int stride = cairo_image_surface_get_stride(surface);

    uint32_t* p = (uint32_t*)(data + stride * y);

    p[x] = pixel;
}
