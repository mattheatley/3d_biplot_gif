library("rgl")
library("optparse")

# http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization

# specify command line arguments
option_list = list(
  make_option(
     c("-i", "--input")
    ,type="character"
    ,default=NULL
    ,help="dataset file path"
    ,metavar="/path/to/file"
    )
  ,make_option(
     c("-o", "--output")
    ,type="character"
    ,default='output'
    ,help="output file name"
    ,metavar="name"
    )
)
 
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser)

input_path <- opt$input

if (is.null(input_path)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

out_name <- opt$output

out_dir <- paste(dirname(input_path),out_name,sep='/')

dir.create(out_dir, showWarnings = FALSE)

pca_scores <- read.table(
   input_path
  ,comment.char = ""
  ,header = TRUE)

x <- PC1 <- pca_scores$PC1
y <- PC2 <- pca_scores$PC2
z <- PC3 <- pca_scores$PC3

# Open a new RGL device
rgl.open()

# specify window size
par3d(windowRect = c(0, 0, 1000, 1000))
# specify plot zoom (larger values make the image smaller)
par3d(zoom = 0.6)
# background color
rgl.bg(color = "white")

# calculate xyz limits
lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}

axes <- rbind( 
     c( lim(x),  c(0, 0), c(0, 0) )
    ,c( c(0, 0), lim(y),  c(0, 0) )
    ,c( c(0, 0), c(0, 0), lim(z)  )
    )

labels = c(
   "PC1"
  ,"PC2"
  ,"PC3"
  )

axis.col = "grey"

# plot axis lines
for (row in 1:nrow(axes)){

  coordinates <- axes[row,]
  
  xr <- coordinates[1:2]
  yr <- coordinates[3:4]
  zr <- coordinates[5:6]

  rgl.lines(
     xr, yr, zr
    ,color = axis.col)

  rgl.points(
     xr[2], yr[2], zr[2]
    ,color = axis.col
    ,size  = 3
    )

  rgl.texts(
     xr[2], yr[2], zr[2]
    ,text = labels[row]
    ,color = axis.col
    ,adj = c(0.5, -0.8)
    ,size = 2
    )
}

# scatter plot
rgl.spheres(
   x, y, z
  ,r=3
  ,color=pca_scores$COLOR
  )

# ellipsoids
for (group in unique(pca_scores$CLUSTER)){

  subset <- subset(pca_scores, CLUSTER == group)

  xs <- subset$PC1
  ys <- subset$PC2
  zs <- subset$PC3
  
  fill <- unique(subset$COLOR)

  ellips <- ellipse3d(
     cov(cbind(xs,ys,zs))
    ,centre=c(mean(xs), mean(ys), mean(zs))
    ,level = 0.95
    )

  plot3d(
     ellips
    ,col = fill
    ,alpha = 0.1
    ,add = TRUE
    ,box = FALSE
    ,type = "shade"
    )

  texts3d(
     mean(xs)
    ,mean(ys)+20
    ,mean(zs)
    ,group
    ,color=fill
    )
  }

# create 3d gif
movie3d(
  spin3d(
     axis=c(0,1,0)
    ,rpm=3
    )
  ,fps= 10
  ,duration=20
  ,movie=out_name
  ,dir=out_dir
  ,convert=TRUE
  )
