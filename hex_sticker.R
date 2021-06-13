library(hexSticker) # Create Hexagon Sticker in R
library(showtext)   # Using Fonts More Easily in R Graphs

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("PT Sans")

sticker(
  # Subplot (image)
  subplot = "logo_bar.png",       # Image name
  s_y = 0.7,                          # Position of the sub plot (y)
  s_x = 1.05,                       # Position of the sub plot (x)
  s_width = 0.8,                   # Width of the sub plot
  s_height=1.4,                    # Height of the sub plot
  # Font
  package = "CaviR",            # Package name (will be printed on the sticker)
  p_size = 14,                       # Font size of the text
  p_y = 1.32,                        # Position of the font (y)
  p_x=1,                         # Position of the font (x)
  p_color="#2F3D4E",
  p_family = "PT Sans",               # Defines font
  # Spotlight
  spotlight = FALSE,                 # Enables spotlight
  l_y=0.8,                          # Position of spotlight (y)
  l_x=0.7,                          # Position of spotlight (x)
  # Sticker colors
  h_fill = "#f5f7fa",               # Color for background
  h_color = "#2F3D4E",              # Color for border
  # Resolution
  dpi=1200,                         # Sets DPI
  # Save
  filename="logo.png"               # Sets file name and location where to store the sticker
)
