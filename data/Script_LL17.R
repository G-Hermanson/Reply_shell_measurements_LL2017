###Clear workspace

	rm( list = ls() )

library(ape)
library(nnet)
library(mda)
library(geiger)
library(Claddis)
source('MS11_pFDA_script.R') #pFDA script by Motani & Schmitz (2011)
	
	set.seed(7)
		
#Set wd to path where data is stored
	
#setwd( )
	
#Load data	
specimen.info <- read.csv( "Lichtig_Lucas_2017_data.csv", header = TRUE )

#Delete certain fossils, for which the values were likely incorrect, but for which we could not generate credible values with certainty
specimen.info <- specimen.info[ -which(specimen.info$ecology == "fossil" & specimen.info$Species != "Proganochelys_quenstedti" & specimen.info$Species != "Meiolania_platyceps" & specimen.info$Species != "Proterochersis_robusta" & !grepl("Basilemys", specimen.info$Species ) ) ,]	

rownames( specimen.info ) <- specimen.info[ , "Unique_ID"]
		specimen.info$numbers.for.plot <- seq(1:length(rownames(specimen.info)))		
				specimen.info$unique.species.numbers <- as.numeric(as.factor(c(specimen.info$Species)))

head(specimen.info)	
	
#Modify ecological assigned of several species	
	species.with.unassigned.ecologies <- as.character(unique( specimen.info$Species[which(specimen.info$ecology == "not_assigned") ] ))
		#Rhinoclemmys areolata is actually terrestrial
		specimen.info$ecology[which(specimen.info$Species == species.with.unassigned.ecologies[1])] <- "terrestrial"
		#Rhinoclemmys_funerea is actually aquatic
		specimen.info$ecology[which(specimen.info$Species == species.with.unassigned.ecologies[2])] <- "aquatic"
	
#Colour sheme for ecological categories
	ecology.bg <- c( "dodgerblue3" , "tan" , "#D4145A")
		names( ecology.bg ) <- c( "aquatic" , "terrestrial" , "fossil" )
			ecology.bg <- ecology.bg[ as.character( specimen.info[ , "ecology" ] ) ]

#Colour sheme for numbers that identify data points
	number.colour <- c("white" , "black" , "black")
		names( number.colour ) <- c( "aquatic" , "terrestrial" , "fossil" )
			number.colour <- number.colour[ as.character( specimen.info[ , "ecology" ] ) ]
			
	#Correct their assignments of Malacochersus tornieri 
	ecology.bg[ match("Malacochersus_tornieri", specimen.info$Species) ] <- "tan"
	number.colour[ match("Malacochersus_tornieri", specimen.info$Species) ] <- "black"
	
	xx <- specimen.info[ specimen.info$ecology == "fossil" , "numbers.for.plot"]
	yy <- specimen.info[ specimen.info$Species == "Malacochersus_tornieri" , "numbers.for.plot"]
	zz <- specimen.info[ specimen.info$Species == "Terrapene_coahuila" , "numbers.for.plot"]
	oo <- specimen.info[ specimen.info$Species == "Homopus_boulengeri" , "numbers.for.plot"]
		specific.data.point.labels <- c(xx,yy,zz,oo)
		specific.data.point.labels.names <- specimen.info[ specific.data.point.labels, "Species" ]
		specific.data.point.label.positions <- c(2,2,1,1,3,3,4,4,3,1,1,3,3)
								   
										   
#Excluding original fossil measurement of Lichtig & Lucas such that coronal doming is only shown for 'true' measurements
	specimen.info_to_keep <- 	specimen.info[- 	 which(grepl("Basilemys_gaffneyi",specimen.info$Unique_ID)	| grepl("platyceps_Unknown",specimen.info$Unique_ID) | grepl("quenstedti_Unknown",specimen.info$Unique_ID) | grepl("robusta_Unknown",specimen.info$Unique_ID) ) , ]	

#Colour sheme for ecological categories
	ecology.bg.coronal <- c( "dodgerblue3" , "tan" , "#D4145A")
		names( ecology.bg.coronal ) <- c( "aquatic" , "terrestrial" , "fossil" )
			ecology.bg.coronal <- ecology.bg.coronal[ as.character( specimen.info_to_keep[ , "ecology" ] ) ]

#Colour sheme for numbers that identify data points
	number.colour.coronal <- c("white" , "black" , "black")
		names( number.colour.coronal ) <- c( "aquatic" , "terrestrial" , "fossil" )
			number.colour.coronal <- number.colour.coronal[ as.character( specimen.info_to_keep[ , "ecology" ] ) ]
			
	#Correct their assignments of Malacochersus tornieri 
	ecology.bg.coronal[ match("Malacochersus_tornieri", specimen.info_to_keep $Species) ] <- "tan"
	number.colour.coronal[ match("Malacochersus_tornieri", specimen.info_to_keep $Species) ] <- "black"

	xx <- specimen.info_to_keep[ specimen.info_to_keep$Source == "new_measurement_this_study" , "numbers.for.plot"]
	yy <- specimen.info_to_keep[ specimen.info_to_keep$Species == "Malacochersus_tornieri" , "numbers.for.plot"]
	zz <- specimen.info_to_keep[ specimen.info_to_keep$Species == "Terrapene_coahuila" , "numbers.for.plot"]
	oo <- specimen.info_to_keep[ specimen.info_to_keep$Species == "Homopus_boulengeri" , "numbers.for.plot"]
		specific.data.point.labels.coronal <- c(xx,yy,zz,oo)
		specific.data.point.labels.names.coronal <- specimen.info[ specific.data.point.labels.coronal, "Species" ]
		specific.data.point.label.positions.coronal <- c(2,1,3,4,3,1,1,3,3)		
	
#Reproduce their plot	
	
	pdf('sagittal_doming_left_coronal_doming_right.pdf', useDingbats = F, width = 18, height = 9)
	
	#dev.new(width = 18, height = 9)
	par(mfrow = c(1,2))
	
	#Sagittal doming plot
	plot(specimen.info$Length.height ~ specimen.info$Carapace.plastron_width, pch = 21, bg = ecology.bg, cex = 2.1, xlab = "Carapace width-plastron width ratio" , ylab = "Carapace length-height ratio (sagittal doming)" ,)	
		#Below line for text identifying specimens
		#text( specimen.info$Length.height ~ specimen.info$Carapace.plastron_width , labels = specimen.info$numbers.for.plot , col = number.colour,  cex = 0.5 )
		#Below line for text identifying species
		text( specimen.info$Length.height ~ specimen.info$Carapace.plastron_width , labels = specimen.info$unique.species.numbers , col = number.colour,  cex = 0.5 )
		#Labelling specific data points
		text( specimen.info$Length.height[specific.data.point.labels] ~ specimen.info$Carapace.plastron_width[specific.data.point.labels] , labels = specific.data.point.labels.names , col = "black",  cex = 0.8 , pos = specific.data.point.label.positions )
		
		legend( "topleft" , legend = c( "aquatic" , "terrestrial" , "fossil") , pch = 21 , pt.bg = c( "dodgerblue3" , "tan" , "#D4145A") , cex = 1.2 , pt.cex = 2.5 , bty = "n" )

	#Coronoal doming plot
	plot(specimen.info_to_keep$Carapace_width_height ~ specimen.info_to_keep$Carapace.plastron_width, pch = 21, bg = ecology.bg.coronal, cex = 2.1, xlab = "Carapace width-plastron width ratio" , ylab = "Carapace width-height ratio (coronal doming)" )	
		#Below line for text identifying specimens
		#text( specimen.info_to_keep$Carapace_width_height ~ specimen.info_to_keep$Carapace.plastron_width , labels = specimen.info_to_keep$numbers.for.plot , col = number.colour.coronal,  cex = 0.5 )
		#Below line for text identifying species
		text( specimen.info_to_keep$Carapace_width_height ~ specimen.info_to_keep$Carapace.plastron_width , labels = specimen.info_to_keep$unique.species.numbers , col = number.colour.coronal,  cex = 0.5 )
		#Labelling specific data points
		text( specimen.info$Carapace_width_height[specific.data.point.labels.coronal] ~ specimen.info$Carapace.plastron_width[specific.data.point.labels.coronal] , labels = specific.data.point.labels.names.coronal , col = "black",  cex = 0.8 , pos = specific.data.point.label.positions.coronal )
		
		legend( "topleft" , legend = c( "aquatic" , "terrestrial" , "fossil" ) , pch = 21 , pt.bg = c( "dodgerblue3" , "tan" , "#D4145A") , cex = 1.2 , pt.cex = 2.5 , bty = "n" )

dev.off()		
		
# For figure caption (numbers)
cat('\n',paste( unique( paste0( specimen.info$unique.species.numbers , "- ", gsub("_", " ", specimen.info$Species)) ) , collapse= "; "))
				
				
#Get species mean values for species
	#Delete unreliable fossil measurements (Basilemys gaffneyi, the original Proterochersis robusta, Proganochelys quenstedti and Meiolania platyceps)		

#Get mean carapace width-plastron width ratio

CW_PW_ratio <- c()

unique.species <- unique(unlist( lapply(lapply(lapply( strsplit(specimen.info_to_keep$Unique_ID,"_" ),unlist) , function(x) x[c(1,2)] ) , paste , collapse="_" )))

for ( i in 1:nrow(specimen.info_to_keep)){
  for ( j in 1:length(unique.species)){
    
    sp.temp <- unique.species[j]

      CW_PW_ratio[j] <- mean ( specimen.info_to_keep[ grepl(sp.temp,specimen.info_to_keep$Species) , 4 ]   )
      
    }
  }

names(CW_PW_ratio) <- unique.species

#Get mean coronal doming ratio

coronal_doming_ratio <- c()

for ( i in 1:nrow(specimen.info_to_keep)){
  for ( j in 1:length(unique.species)){
    
    sp.temp <- unique.species[j]
    
    coronal_doming_ratio[j] <- mean ( specimen.info_to_keep[ grepl(sp.temp,specimen.info_to_keep$Species) , 9 ]   )
    
  }
}

names(coronal_doming_ratio) <- unique.species

#Data frame with measurements
doming_data <- data.frame(CW_PW_ratio, coronal_doming_ratio)
  doming_data$ecology <- NA

for ( i in 1:nrow(specimen.info_to_keep)){
  for ( j in 1:length(unique.species)){
    
    sp.temp <- unique.species[j]
    
    doming_data$ecology[j] <- specimen.info_to_keep[ grepl(sp.temp,specimen.info_to_keep$Species) , "ecology" ] [1]
    
  }
  
}

  doming_data <- doming_data[complete.cases(doming_data),]
doming_data

#Read Thomson trees
#Using a sample of calibrated trees (100 trees from Thomson et al. 2021)

#function retrieved from https://stackoverflow.com/questions/57680924/changing-tip-labels-for-a-multiphylo-object
rename.tips.phylo <- function(tree, names) {
  tree$tip.label <- names
  return(tree)
}


##### Get trees from Thomson et al. (2021) to run iterative pFDA ####
thomson_trees <- read.nexus("Thomson100_fixed.tre")

#get binomial species names
tree.spp <-  lapply(lapply(  lapply(lapply ( thomson_trees , function(x) strsplit(x$tip.label,"_") ),
                                    function(x) lapply(x , function(x) x[c(1,2)]) ) ,
                             function(x) lapply( x , paste, collapse="_")  ) , 
                    function(x) unlist(x))

for ( i in 1:length(thomson_trees)){
  
  thomson_trees[[i]]$tip.label.new <- NULL
  thomson_trees[[i]]$tip.label.new <- tree.spp[[i]]
  
}

#store new trees
trees.new <- list()
for ( i in 1:length(thomson_trees)){
  trees.new[[i]] <- rename.tips.phylo(thomson_trees[[i]] , tree.spp[[i]]) 
  
}

class(trees.new) <- "multiPhylo"

#which tip labels are actually in the data frame

extant_doming_data <- doming_data[rownames(doming_data) %in% trees.new[[1]]$tip.label ,]
extinct_doming_data <- doming_data[!rownames(doming_data) %in% rownames(extant_doming_data) & doming_data$ecology=="fossil" ,]

#add fossil tips to Thomson trees

scaled_trees_doming <- list()

for ( i in 1:length(trees.new)){
  
  tree.temp <- keep.tip(trees.new[[i]] , rownames(extant_doming_data))
  
  tree.temp$root.time <- max(diag(vcv(tree.temp)))
  #tree.temp$root.time <- ifelse(tree.temp$root.time<220, 220,
   #                             tree.temp$root.time)
  
  tree.temp$root.edge <- ifelse(tree.temp$root.time<220, 220-tree.temp$root.time+1 ,
                              1)

  tree.temp <- bind.tip(tree.temp ,tip.label="Meiolania_platyceps",
                        edge.length= max(diag(vcv(tree.temp)))-0.012 ,
                        
                        where=(Ntip(tree.temp)+1), 
                        position=1)
  
  tree.temp$root.time <- max(diag(vcv(tree.temp)))
  tree.temp$root.edge <- ifelse(tree.temp$root.time<220, 220-tree.temp$root.time ,
                                1)
  
  
  tree.temp <- bind.tip(tree.temp,tip.label = "Proterochersis_robusta",
                        edge.length = ifelse(tree.temp$root.time>220,
                                             tree.temp$root.time-201, 15) ,
                        
                        where=(Ntip(tree.temp)+1), 
                        position= 1
                          )
  
  tree.temp$root.time <- max(diag(vcv(tree.temp)))
  
  tree.temp$root.edge <- 1
  
  tree.temp <- bind.tip(tree.temp,tip.label = "Proganochelys_quenstedti",
                        edge.length = ifelse(tree.temp$root.time > 220,
                                             tree.temp$root.time-201, 15) ,
                        
                        where=(Ntip(tree.temp)+1), 
                        position=1 )
  
  tree.temp$root.time <- max(diag(vcv(tree.temp)))
  
  
  node.temp <- date_nodes(tree.temp)[mrca(tree.temp)["Apalone_spinifera","Carettochelys_insculpta"]]
  length.temp <- node.temp - 66
  
  tree.temp <- bind.tip(tree.temp,tip.label = "Basilemys_morrinensis",
                        edge.length = length.temp,
                        where=getMRCA(tree.temp,c("Apalone_spinifera","Carettochelys_insculpta")),
                        position= 1)
  
  
  tree.temp <- bind.tip(tree.temp,tip.label = "Basilemys_varialosa",
                        edge.length = 4,
                        where=which(tree.temp$tip.label == 'Basilemys_morrinensis'),
                        position= 10)
  
  scaled_trees_doming[[i]] <- tree.temp
  
  setTxtProgressBar(txtProgressBar(0,length(trees.new),style = 3),i)
  
}

class(scaled_trees_doming) <- "multiPhylo"

dev.new()
plot(scaled_trees_doming[[1]], cex = 0.5)

#pFDA with 100 iterations

reps =  length(scaled_trees_doming)
pFDA.doming <- list()
lambda.doming <- list()

for ( i in 1:reps){
  
  doming_data.temp <- doming_data[scaled_trees_doming[[i]]$tip.label,]
  XA.temp <- doming_data.temp[,1:2] 
  treA.temp <- scaled_trees_doming[[i]]
  #if(!is.binary.tree(treA)) treA <- multi2di(treA, random = TRUE) 
  #is.ultrametric(treA)
  
  ddA.temp <- XA.temp[treA.temp$tip.label,]
  taxaA.temp <- rownames(ddA.temp)
  gA.temp <- as.factor(doming_data.temp$ecology)
  
  testtaxa.temp <- rownames(ddA.temp[gA.temp=="fossil",])
  testtaxan.temp <- row(ddA.temp)[gA.temp=="fossil",1]
  trainingtaxa.temp <- rownames(ddA.temp[-testtaxan.temp,])
  X.temp <- XA.temp[-testtaxan.temp,]
  dd.temp <- ddA.temp[-testtaxan.temp,]
  g.temp <- gA.temp[-testtaxan.temp]
  g.temp <- droplevels(g.temp)
  tre.temp <- drop.tip(treA.temp, testtaxa.temp) 
  
  #lambda
  ol1.temp <- optLambda(X.temp,g.temp,tre.temp)
  lambda.temp <- ol1.temp$optlambda[1,1]
  
  lambda.doming[[i]] <- lambda.temp
  
  #pfda
  optl.temp <- ol1.temp$optlambda[1,1]
  pFDA.temp <- phylo.fda.pred(XA.temp,gA.temp,taxaA.temp,
                              treA.temp,
                              testtaxan.temp,
                              val=optl.temp,eqprior = T)
  
  pFDA.doming[[i]] <- pFDA.temp
  
  setTxtProgressBar(txtProgressBar(0,reps,style = 3),i)
  
}

#lambda variation
dev.new()
hist(unlist(lambda.doming),xlim=c(0,1))

#Predictions
doming_predictions <- lapply(pFDA.doming, 
                             function(x) predict(x, newdata=x$DATAtest,
                             type="posterior"))
doming_predictions <- lapply ( doming_predictions , 
                               function(x) {rownames(x) <- testtaxa.temp ; x})

#Predicted medians
doming_predictions_median <- round( rbind( apply( do.call(rbind, lapply ( doming_predictions ,  function(x) x[1,] )) , 2 , function(x) summary(x)[c(1,3,6)] ) , 
                                   apply( do.call(rbind, lapply ( doming_predictions ,  function(x) x[2,] )) , 2 ,function(x) summary(x)[c(1,3,6)]),
                                   apply( do.call(rbind, lapply ( doming_predictions ,  function(x) x[3,] )) , 2 ,function(x) summary(x)[c(1,3,6)]),
                                   apply( do.call(rbind, lapply ( doming_predictions ,  function(x) x[4,] )) , 2 ,function(x) summary(x)[c(1,3,6)]),
                                   apply( do.call(rbind, lapply ( doming_predictions ,  function(x) x[5,] )) , 2 ,function(x) summary(x)[c(1,3,6)])) , 3)

rownames(doming_predictions_median) <- paste0(rep(sort(rownames(extinct_doming_data)),each=3) ,'_', c('min','median','max'))
doming_predictions_median

# number of trees in which each taxon was predicted each ecology

sapply(1:nrow(extinct_doming_data) , 
       function(sp) table(unlist(lapply ( pFDA.doming , function(x) x$testprediction[sp,]))))

##success rates for extant
doming_predictions_extant <- lapply ( pFDA.doming , function(x) predict(x, type="class"))
doming_predictions_extant <- lapply(doming_predictions_extant , function(x){names(x) <- trainingtaxa.temp; x})

# % correct "aquatic" classifications
summary(unlist(lapply ( doming_predictions_extant ,
                        function(x) mean(rownames(dd.temp)[which(doming_data.temp$ecology=="aquatic")] %in% names(x)[which(x=="aquatic")])  )))[c(1,3,6)]

# % correct "terrestrial" classifications
summary(unlist(lapply ( doming_predictions_extant ,
                        function(x) mean(rownames(dd.temp)[which(doming_data.temp$ecology=="terrestrial")] %in% names(x)[which(x=="terrestrial")])  )))[c(1,3,6)]


overall_sucess <- summary( unlist(lapply ( pFDA.doming, 
                           function(x) as.numeric(attributes(print(x$confusion))$error) )))

overall_sucess <- 1-overall_sucess[c(1,3,6)]
overall_sucess