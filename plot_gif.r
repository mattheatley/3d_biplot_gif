library("rgl")
library("optparse")

# http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization

# calculate xyz limits
lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}

# specify command line arguments
option_list = list(
  make_option(
     c("-i", "--input")
    ,type="character"
    ,default=NULL
    ,help="score file path"
    ,metavar="/path/to/file"
    )
  ,make_option(
     c("-a", "--arrows")
    ,type="character"
    ,default=NULL
    ,help="arrow file path"
    ,metavar="/path/to/file"
    )
  ,make_option(
     c("-l", "--labels")
    ,action="store_true"
    ,default=FALSE
    ,help="label clusters & loadings"
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

height <- lim(y)[2]*2

scale_base <- height/150


# Open a new RGL device
rgl.open()

# specify window size
par3d(windowRect = c(0, 0, 1000, 1000))
# specify plot zoom (larger values make the image smaller)
par3d(zoom = 0.6)
# background color
rgl.bg(color = "white")

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


arrow.col <- 'black'

if (!(is.null(opt$arrows))){

  arrow_coords <- read.table(
     opt$arrows
    ,comment.char = ""
    ,header = TRUE
    )


  # scale calculation from pca module
  max_arrow <- max(
     max(abs(arrow_coords$PC1))
    ,max(abs(arrow_coords$PC2))
    ,max(abs(arrow_coords$PC3))
    )

  arrow_scale  <- max( 1, round( (height/2)/max_arrow, 2 ) ) * 0.5

  for (row in 1:nrow(arrow_coords)){

    coordinates <- arrow_coords[row,]

    xa <- coordinates$PC1*arrow_scale
    ya <- coordinates$PC2*arrow_scale
    za <- coordinates$PC3*arrow_scale

    # arrows
    arrow <- arrow3d(
       p0 = c(0,  0,  0 )
      ,p1 = c(xa, ya, za)
      ,type = c("rotation")
      ,barblen=0.02
      ,theta=270
      ,width=0.2
      ,col=arrow.col
      
      ,n=40
      ,plot=TRUE
      )


    if (!(is.null(opt$labels))){

      texts3d(
        mean(xa)
       ,mean(ya)+mean(ya)*0.1
       ,mean(za)
       ,coordinates$VARIABLE
       ,color=arrow.col
       )
    }
  }
}


for (group in unique(pca_scores$SHAPE)){

  subset <- subset(pca_scores, SHAPE == group)

  xs <- subset$PC1
  ys <- subset$PC2
  zs <- subset$PC3
  
  size = scale_base

  if (group == 'CIRCLE'){
    spheres3d(
       xs, ys, zs
      ,r=size
      ,color=subset$COLOR
      )
  } else {

    if (group == 'TRIANGLE'){
      shape <- tetrahedron3d()
    }

    if (group == 'SQUARE'){
      shape <- cube3d()
    }

    if (group == 'DIAMOND'){
      shape <- octahedron3d()
      size=size*1.25
    }

  # scatter plot
  shapelist3d(shapes=shape,
     xs, ys, zs
    ,size=size
    ,color=subset$COLOR
    )




  }

}


# ellipsoids
for (group in unique(pca_scores$CLUSTER)){

  if (!(group == 'No_Consensus') ){

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

    }

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
