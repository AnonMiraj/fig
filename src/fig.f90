module fig
   use fig_rgb
   use fig_rgb_color_constants
   use fig_config
   use fig_shapes
   use fig_path
   use fig_drawing, only: drawing
   use fig_svg, only: svg_canvas
   use fig_bitmap, only: bitmap_canvas
   implicit none
   
contains 

   subroutine draw_to_svg(draw, width, height, file_name)
       integer,intent(in) :: width,height
       type(drawing), intent(in)       :: draw
       character(len=*), intent(in)    :: file_name
       type(svg_canvas):: svg_canva
       call svg_canva%init(width, height, file_name)
       call svg_canva%apply_shapes(draw)
       call svg_canva%save_to_svg()
       call svg_canva%destroy()
   end subroutine draw_to_svg

   subroutine draw_to_png(draw, width, height, file_name)
       type(drawing), intent(in)       :: draw
       integer,intent(in) :: width,height
       character(len=*), intent(in)    :: file_name
       type(bitmap_canvas):: bitmap_canva
       call bitmap_canva%init(width, height, file_name)
       call bitmap_canva%apply_shapes(draw)
       call bitmap_canva%save_to_png()
       call bitmap_canva%destroy()
   end subroutine draw_to_png


end module fig

