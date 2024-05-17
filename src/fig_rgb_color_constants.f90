module fig_rgb_color_constants
   use fig_rgb
   implicit none
   
   type(RGB), parameter :: FIG_COLOR_ALICEBLUE	= RGB(240, 248, 255, 255)
   type(RGB), parameter :: FIG_COLOR_ANTIQUEWHITE = RGB(250, 235, 215, 255)
   type(RGB), parameter :: FIG_COLOR_AQUA = RGB( 0, 255, 255, 255)
   type(RGB), parameter :: FIG_COLOR_AQUAMARINE = RGB(127, 255, 212, 255)
   type(RGB), parameter :: FIG_COLOR_AZURE = RGB(240, 255, 255, 255)
   type(RGB), parameter :: FIG_COLOR_BEIGE = RGB(245, 245, 220, 255)
   type(RGB), parameter :: FIG_COLOR_BISQUE = RGB(255, 228, 196, 255)
   type(RGB), parameter :: FIG_COLOR_BLACK = RGB( 0, 0, 0, 255)
   type(RGB), parameter :: FIG_COLOR_BLANCHEDALMOND= RGB(255, 235, 205, 255)
   type(RGB), parameter :: FIG_COLOR_BLUE = RGB( 0, 0, 255, 255)
   type(RGB), parameter :: FIG_COLOR_BLUEVIOLET = RGB(138, 43, 226, 255)
   type(RGB), parameter :: FIG_COLOR_BROWN = RGB(165, 42, 42, 255)
   type(RGB), parameter :: FIG_COLOR_BURLYWOOD = RGB(222, 184, 135, 255)
   type(RGB), parameter :: FIG_COLOR_CADETBLUE= RGB( 95, 158, 160, 255)
   type(RGB), parameter :: FIG_COLOR_CHARTREUSE = RGB(127, 255, 0, 255)
   type(RGB), parameter :: FIG_COLOR_CHOCOLATE = RGB(210, 105, 30, 255)
   type(RGB), parameter :: FIG_COLOR_CORAL = RGB(255, 127, 80, 255)
   type(RGB), parameter :: FIG_COLOR_CORNFLOWERBLUE = RGB(100, 149, 237, 255)
   type(RGB), parameter :: FIG_COLOR_CORNSILK = RGB(255, 248, 220, 255)
   type(RGB), parameter :: FIG_COLOR_CRIMSON = RGB(220, 20, 60, 255)
   type(RGB), parameter :: FIG_COLOR_CYAN = RGB( 0, 255, 255, 255)
   type(RGB), parameter :: FIG_COLOR_DARKBLUE = RGB( 0, 0, 139, 255)
   type(RGB), parameter :: FIG_COLOR_DARKCYAN = RGB( 0, 139, 139, 255)
   type(RGB), parameter :: FIG_COLOR_DARKGOLDENROD = RGB(184, 134, 11, 255)
   type(RGB), parameter :: FIG_COLOR_DARKGRAY = RGB(169, 169, 169, 255)
   type(RGB), parameter :: FIG_COLOR_DARKGREEN = RGB( 0, 100, 0, 255)
   type(RGB), parameter :: FIG_COLOR_DARKGREY = RGB(169, 169, 169, 255)
   type(RGB), parameter :: FIG_COLOR_DARKKHAKI = RGB(189, 183, 107, 255)
   type(RGB), parameter :: FIG_COLOR_DARKMAGENTA = RGB(139, 0, 139, 255)
   type(RGB), parameter :: FIG_COLOR_DARKOLIVEGREEN = RGB( 85, 107, 47, 255)
   type(RGB), parameter :: FIG_COLOR_DARKORANGE = RGB(255, 140, 0, 255)
   type(RGB), parameter :: FIG_COLOR_DARKORCHID = RGB(153, 50, 204, 255)
   type(RGB), parameter :: FIG_COLOR_DARKED = RGB(139, 0, 0, 255)
   type(RGB), parameter :: FIG_COLOR_DARKSALMON = RGB(233, 150, 122, 255)
   type(RGB), parameter :: FIG_COLOR_DARKSEAGREEN = RGB(143, 188, 143, 255)
   type(RGB), parameter :: FIG_COLOR_DARKSLATEBLUE = RGB( 72, 61, 139, 255)
   type(RGB), parameter :: FIG_COLOR_DARKSLATEGRAY = RGB( 47, 79, 79, 255)
   type(RGB), parameter :: FIG_COLOR_DARKSLATEGREY = RGB( 47, 79, 79, 255)
   type(RGB), parameter :: FIG_COLOR_DARKTURQUOISE = RGB( 0, 206, 209, 255)
   type(RGB), parameter :: FIG_COLOR_DARKVIOLETT = RGB(148, 0, 211, 255)
   type(RGB), parameter :: FIG_COLOR_DEEPPINK = RGB(255, 20, 147, 255)
   type(RGB), parameter :: FIG_COLOR_DEEPSKYBLUE = RGB( 0, 191, 255, 255)
   type(RGB), parameter :: FIG_COLOR_DIMGRAY = RGB(105, 105, 105, 255)
   type(RGB), parameter :: FIG_COLOR_DIMGREY = RGB(105, 105, 105, 255)
   type(RGB), parameter :: FIG_COLOR_DODGERBLUE = RGB( 30, 144, 255)
   type(RGB), parameter :: FIG_COLOR_FIREBRICK = RGB(178, 34, 34, 255)
   type(RGB), parameter :: FIG_COLOR_FLORALWHITE = RGB(255, 250, 240, 255)
   type(RGB), parameter :: FIG_COLOR_FORESTGREEN = RGB( 34, 139, 34, 255)
   type(RGB), parameter :: FIG_COLOR_FUCHSIA = RGB(255, 0, 255, 255)
   type(RGB), parameter :: FIG_COLOR_GAINSBORO = RGB(220, 220, 220, 255)
   type(RGB), parameter :: FIG_COLOR_GHOSTWHITE= RGB(248, 248, 255, 255)
   type(RGB), parameter :: FIG_COLOR_GOLD = RGB(255, 215, 0, 255)
   type(RGB), parameter :: FIG_COLOR_GOLDENROD = RGB(218, 165, 32, 255)
   type(RGB), parameter :: FIG_COLOR_GRAY = RGB(128, 128, 128, 255)
   type(RGB), parameter :: FIG_COLOR_GREY = RGB(128, 128, 128, 255)
   type(RGB), parameter :: FIG_COLOR_GREEN = RGB( 0, 128, 0, 255)
   type(RGB), parameter :: FIG_COLOR_GREENYELLOW = RGB(173, 255, 47, 255)
   type(RGB), parameter :: FIG_COLOR_HONEYDEW = RGB(240, 255, 240, 255)
   type(RGB), parameter :: FIG_COLOR_HOTPINK = RGB(255, 105, 180, 255)
   type(RGB), parameter :: FIG_COLOR_INDIANRED = RGB(205, 92, 92, 255)
   type(RGB), parameter :: FIG_COLOR_INDIGO = RGB( 75, 0, 130, 255)
   type(RGB), parameter :: FIG_COLOR_IVORY = RGB(255, 255, 240, 255)
   type(RGB), parameter :: FIG_COLOR_KHAKI = RGB(240, 230, 140, 255)
   type(RGB), parameter :: FIG_COLOR_LAVENDER = RGB(230, 230, 250, 255)
   type(RGB), parameter :: FIG_COLOR_LAVENDERBLUSH = RGB(255, 240, 245, 255)
   type(RGB), parameter :: FIG_COLOR_LAWNGREEN = RGB(124, 252, 0, 255)
   type(RGB), parameter :: FIG_COLOR_LEMONCHIFFON = RGB(255, 250, 205, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTBLUE = RGB(173, 216, 230, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTCORAL = RGB(240, 128, 128, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTCYAN = RGB(224, 255, 255, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTGOLDENRODYELLOW = RGB(250, 250, 210, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTGRAY = RGB(211, 211, 211, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTGREEN = RGB(144, 238, 144, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTGREY = RGB(211, 211, 211, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTPINK	= RGB(255, 182, 193, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTSALMON = RGB(255, 160, 122, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTSEAGREEN = RGB( 32, 178, 170, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTSKYBLUE = RGB(135, 206, 250, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTSLATEGRAY = RGB(119, 136, 153, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTSLATEGREY = RGB(119, 136, 153, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTSTEELBLUE = RGB(176, 196, 222, 255)
   type(RGB), parameter :: FIG_COLOR_LIGHTYELLOW = RGB(255, 255, 224, 255)
   type(RGB), parameter :: FIG_COLOR_LIME = RGB( 0, 255, 0, 255)
   type(RGB), parameter :: FIG_COLOR_LIMEGREEN = RGB( 50, 205, 50, 255)
   type(RGB), parameter :: FIG_COLOR_LINEN = RGB(250, 240, 230, 255)
   type(RGB), parameter :: FIG_COLOR_MAGENTA =RGB(255, 0, 255, 255)
   type(RGB), parameter :: FIG_COLOR_MAROON  =RGB(128, 0, 0, 255)
   type(RGB), parameter :: FIG_COLOR_MEDIUMAQUAMARINE = RGB(102, 205, 170, 255)
   type(RGB), parameter :: FIG_COLOR_MEDIUMBLUE = RGB( 0, 0, 205, 255)
   type(RGB), parameter :: FIG_COLOR_MEDIUMORCHID = RGB(186, 85, 211, 255)
   type(RGB), parameter :: FIG_COLOR_MEDIUMPURPLE = RGB(147, 112, 219, 255)
   type(RGB), parameter :: FIG_COLOR_MEDIUMSEAGREEN = RGB( 60, 179, 113, 255)
   type(RGB), parameter :: FIG_COLOR_MEDIUMSLATEBLUE = RGB(123, 104, 238, 255)
   type(RGB), parameter :: FIG_COLOR_MEDIUMSPRINGGREEN = RGB( 0, 250, 154, 255)
   type(RGB), parameter :: FIG_COLOR_MEDIUMTURQUOISE = RGB( 72, 209, 204, 255)
   type(RGB), parameter :: FIG_COLOR_MEDIUMVIOLETRED = RGB(199, 21, 133, 255)
   type(RGB), parameter :: FIG_COLOR_MIDNIGHTBLUE = RGB( 25, 25, 112, 255)
   type(RGB), parameter :: FIG_COLOR_MINTCREAM = RGB(245, 255, 250, 255)
   type(RGB), parameter :: FIG_COLOR_MISTYROSE = RGB(255, 228, 225, 255)
   type(RGB), parameter :: FIG_COLOR_MOCCASIN = RGB(255, 228, 181, 255)
   type(RGB), parameter :: FIG_COLOR_NAVAJOWHITE = RGB(255, 222, 173, 255)
   type(RGB), parameter :: FIG_COLOR_NAVY = RGB( 0, 0, 128, 255)
   type(RGB), parameter :: FIG_COLOR_OLDLACE = RGB(253, 245, 230, 255)
   type(RGB), parameter :: FIG_COLOR_OLIVE = RGB(128, 128, 0, 255)
   type(RGB), parameter :: FIG_COLOR_OLIVEDRAB = RGB(107, 142, 35, 255)
   type(RGB), parameter :: FIG_COLOR_ORANGE = RGB(255, 165, 0, 255)
   type(RGB), parameter :: FIG_COLOR_ORANGERED = RGB(255, 69, 0, 255)
   type(RGB), parameter :: FIG_COLOR_ORCHID = RGB(218, 112, 214, 255)
   type(RGB), parameter :: FIG_COLOR_PALEGOLDENROD = RGB(238, 232, 170, 255)
   type(RGB), parameter :: FIG_COLOR_PALEGREEN = RGB(152, 251, 152, 255)
   type(RGB), parameter :: FIG_COLOR_PALETURQUOISE = RGB(175, 238, 238, 255)
   type(RGB), parameter :: FIG_COLOR_PALEVIOLETRED = RGB(219, 112, 147, 255)
   type(RGB), parameter :: FIG_COLOR_PAPAYAWHIP = RGB(255, 239, 213, 255)
   type(RGB), parameter :: FIG_COLOR_PEACHPUF = RGB(255, 218, 185, 255)
   type(RGB), parameter :: FIG_COLOR_PERU = RGB(205, 133, 63, 255)
   type(RGB), parameter :: FIG_COLOR_PINK = RGB(255, 192, 203, 255)
   type(RGB), parameter :: FIG_COLOR_PLUM = RGB(221, 160, 221, 255)
   type(RGB), parameter :: FIG_COLOR_POWDERBLUE = RGB(176, 224, 230, 255)
   type(RGB), parameter :: FIG_COLOR_PURPLE = RGB(128, 0, 128, 255)
   type(RGB), parameter :: FIG_COLOR_RED = RGB(255, 0, 0, 255)
   type(RGB), parameter :: FIG_COLOR_ROSYBROWN = RGB(188, 143, 143, 255)
   type(RGB), parameter :: FIG_COLOR_ROYALBLUE = RGB( 65, 105, 225, 255)
   type(RGB), parameter :: FIG_COLOR_SADDLEBROWN = RGB(139, 69, 19, 255)
   type(RGB), parameter :: FIG_COLOR_SALMON = RGB(250, 128, 114, 255)
   type(RGB), parameter :: FIG_COLOR_SANDYBROWN = RGB(244, 164, 96, 255)
   type(RGB), parameter :: FIG_COLOR_SEAGREEN = RGB( 46, 139, 87, 255)
   type(RGB), parameter :: FIG_COLOR_SEASHELL = RGB(255, 245, 238, 255)
   type(RGB), parameter :: FIG_COLOR_SIENNA = RGB(160, 82, 45, 255)
   type(RGB), parameter :: FIG_COLOR_SILVER = RGB(192, 192, 192, 255)
   type(RGB), parameter :: FIG_COLOR_SKYBLUE = RGB(135, 206, 235, 255)
   type(RGB), parameter :: FIG_COLOR_SLATEBLUE = RGB(106, 90, 205, 255)
   type(RGB), parameter :: FIG_COLOR_SLATEGRAY = RGB(112, 128, 144, 255)
   type(RGB), parameter :: FIG_COLOR_SLATEGREY = RGB(112, 128, 144, 255)
   type(RGB), parameter :: FIG_COLOR_SNOW = RGB(255, 250, 250, 255)
   type(RGB), parameter :: FIG_COLOR_SPRINGGREEN = RGB( 0, 255, 127, 255)
   type(RGB), parameter :: FIG_COLOR_STEELBLUE = RGB( 70, 130, 180, 255)
   type(RGB), parameter :: FIG_COLOR_TAN = RGB(210, 180, 140, 255)
   type(RGB), parameter :: FIG_COLOR_TEAL = RGB( 0, 128, 128, 255)
   type(RGB), parameter :: FIG_COLOR_THISTLE = RGB(216, 191, 216, 255)
   type(RGB), parameter :: FIG_COLOR_TOMATO = RGB(255, 99, 71, 255)
   type(RGB), parameter :: FIG_COLOR_TURQUOISE = RGB( 64, 224, 208, 255)
   type(RGB), parameter :: FIG_COLOR_VIOLET = RGB(238, 130, 238, 255)
   type(RGB), parameter :: FIG_COLOR_WHEAT = RGB(245, 222, 179, 255)
   type(RGB), parameter :: FIG_COLOR_WHITE = RGB(255, 255, 255, 255)
   type(RGB), parameter :: FIG_COLOR_WHITESMOKE = RGB(245, 245, 245, 255)
   type(RGB), parameter :: FIG_COLOR_YELLOW = RGB(255, 255, 0, 255)
   type(RGB), parameter :: FIG_COLOR_YELLOWGREEN = RGB(154, 205, 50, 255)        

end module fig_rgb_color_constants
