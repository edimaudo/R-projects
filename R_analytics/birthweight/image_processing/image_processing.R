
library(magick)

#reading a png image frink image
frink <- image_read("https://jeroen.github.io/images/frink.png")
#reading a jpg image ex-President Barack Obama from Wikipedia
obama <- image_read('https://upload.wikimedia.org/wikipedia/commons/thumb/8/8d/President_Barack_Obama.jpg/800px-President_Barack_Obama.jpg')

#image details
image_info(obama)
image_info(frink)


#Rendering JPG image into SVG
image_write(obama, path = 'obama.svg', format = 'svg')

#Applying Charcoal effect to Obama's image 
#and compositing it with frink's image
#and finally annotating it with a text
image_charcoal(obama) %>% 
  image_composite(frink)  %>%
  image_annotate("CONFIDENTIAL", size = 50, color = "red", boxcolor = "pink",
                 degrees = 30, location = "+100+100") %>%
  image_rotate(30) %>%
  image_write('obama_with_frink.png','png')