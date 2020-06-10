# 3D Scatter plot


library(scatterplot3d)

scatterplot3d(vehicle$lh,vehicle$lc,vehicle$mc)

# Contour plot

filled.contour(volcano,color=terrain.colors,asp = 1,plot.axes = contour(volcano,add = T))

# 3D Surface plot

persp(volcano,theta = 25,phi = 30,expand = 0.5,col = 'lightblue')
