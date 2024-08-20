program logo_path_test
   use fig
   use fig_test
   implicit none
   integer, parameter :: CANVAS_WIDTH = 200
   integer, parameter :: CANVAS_HEIGHT = 200
   character(len=:), allocatable  :: file_name
   type(drawing) :: canva
   type(path) :: my_path
   type(svg_canvas) :: svg_canva
   type(bitmap_canvas) :: bitmap_canva

   FIG_ABSOLUTE_COORDINATES = .true.
   call canva%init()
   file_name = "fortran_logo"
   my_path%path_string="m 66.01647,123.09029 &
       c 0,-2.98216 0.0232,-3.24783 0.284142,-3.25337 &
       c 0.156278,-0.003 1.698346 ,-0.0723 3.426817,-0.15338 &
       c 2.783528,-0.13052 3.227886,-0.19014 3.888311,-0.52167 &
       c 0.522164,-0.26212 0.845509,-0.56896 1.07887,-1.02377&
       c 0.330889,-0.6449 0.333235,-0.79405 0.333235,-21.18891 &
       c 0,-19.16494 -0.0188,-20.573793 -0.280899,-21.052796&
       c -0.35488,-0.648562 -0.970461,-1.135802 -1.726889,-1.366854 &
       c -0.324547,-0.09913 -2.033124,-0.219157 -3.796837,-0.266719&
       l -3.20675,-0.08648 V 70.949727 V 67.72311 &
       h 27.277676 &
       h 27.277674 &
       v 12.09635 &
       v 12.09635 &
       l -2.88201,-0.01619 &
       c -1.58511,-0.0089 -3.06468,-0.06699 -3.28794,-0.129079 &
       c -0.39914,-0.111009 -0.40916,-0.154768 -0.60046,-2.62228 &
       c -0.24553,-3.166944 -0.44624,-4.569394 -0.9554,-6.675976 &
       c -1.10082,-4.554462 -3.42356,-7.032127 -7.21648,-7.697813 &
       c -1.59136,-0.279295 -8.534273,-0.549154 -14.243197,-0.553608 &
       l -3.937405,-0.0031 &
       v 9.095131 &
       v 9.095131 &
       l 2.070182,-0.102679 &
       c 2.366865,-0.117396 4.22742,-0.40322 5.125527,-0.787403 &
       c 0.95651,-0.409167 1.9665,-1.442775 2.43932,-2.496357 &
       c 0.49398,-1.100746 0.8172,-2.547274 1.07659,-4.818245 &
       c 0.10201,-0.893019 0.21923,-1.788068 0.2605,-1.988997 &
       c 0.0747,-0.363787 0.0884,-0.365326 3.238103,-0.365326 &
       h 3.16306 &
       v 14.531856 &
       v 14.531855 &
       h -3.22343 &
       h -3.223423 &
       l -0.10838,-1.67638 &
       c -0.19627,-3.0357 -0.78736,-5.91064 -1.49764,-7.28416 &
       c -0.97089,-1.87752 -2.88173,-2.721509 -6.722534,-2.969276 &
       c -0.848368,-0.05473 -1.784855,-0.137083 -2.081083,-0.183012 &
       l -0.538596,-0.08351 &
       l 0.06292,9.142548 &
       c 0.06636,9.64289 0.09513,10.01966 0.826875,10.82774 &
       c 0.752359,0.83085 1.191043,0.88792 8.833448,1.14929 &
       c 0.15537,0.005 0.20296,0.76836 0.20296,3.25429 &
       v 3.24734 &
       H 81.684896 &
       H 66.01647 &
       &Z"
   my_path%fill_color = FIG_COLOR_WHITE
   my_path%stroke_color = FIG_COLOR_PURPLE
   call canva%add_shape(my_path)

   call bitmap_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT, file_name)
   call bitmap_canva%apply_shapes(canva)
   call bitmap_canva%save_to_png()
   call bitmap_canva%save_to_ppm()
   call bitmap_canva%destroy()

   call svg_canva%init(CANVAS_WIDTH, CANVAS_HEIGHT, file_name)
   call svg_canva%apply_shapes(canva)
   call svg_canva%save_to_svg()
   call svg_canva%destroy()
   call test_both(file_name)

end program logo_path_test
