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
	
#Load data	
specimen.info <- read.csv( "Lichtig_Lucas_2017_data.csv", header = TRUE )

rownames( specimen.info ) <- specimen.info[ , "Unique_ID"]
		specimen.info$numbers.for.plot <- seq(1:length(rownames(specimen.info)))		
				specimen.info$unique.species.numbers <- as.numeric(as.factor(c(specimen.info$Species)))

head(specimen.info)	
	

#Colour sheme for ecological categories
	ecology.bg <- c( "dodgerblue3" , "tan" , "#D4145A")
		names( ecology.bg ) <- c( "aquatic" , "terrestrial" , "fossil" )
			ecology.bg <- ecology.bg[ as.character( specimen.info[ , "ecology" ] ) ]

#Colour sheme for numbers that identify data points
	number.colour <- c("white" , "black" , "black")
		names( number.colour ) <- c( "aquatic" , "terrestrial" , "fossil" )
			number.colour <- number.colour[ as.character( specimen.info[ , "ecology" ] ) ]
			
	xx <- specimen.info[ specimen.info$ecology == "fossil" , "numbers.for.plot"]
	yy <- specimen.info[ specimen.info$Species == "Malacochersus_tornieri" , "numbers.for.plot"]
	zz <- specimen.info[ specimen.info$Species == "Terrapene_coahuila" , "numbers.for.plot"]
	oo <- specimen.info[ specimen.info$Species == "Homopus_boulengeri" , "numbers.for.plot"]
		specific.data.point.labels <- c(xx,yy,zz,oo)
		specific.data.point.labels.names <- specimen.info[ specific.data.point.labels, "Species" ]
		specific.data.point.label.positions <- c(2,2,1,1,3,3,4,4,3,1,1,3,3,1)
								   

		
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
			
	xx <- specimen.info_to_keep[ specimen.info_to_keep$Source == "new_measurement_this_study" , "numbers.for.plot"]
	yy <- specimen.info_to_keep[ specimen.info_to_keep$Species == "Malacochersus_tornieri" , "numbers.for.plot"]
	zz <- specimen.info_to_keep[ specimen.info_to_keep$Species == "Terrapene_coahuila" , "numbers.for.plot"]
	oo <- specimen.info_to_keep[ specimen.info_to_keep$Species == "Homopus_boulengeri" , "numbers.for.plot"]
		specific.data.point.labels.coronal <- c(xx,yy,zz,oo)
		specific.data.point.labels.names.coronal <- specimen.info[ specific.data.point.labels.coronal, "Species" ]
		specific.data.point.label.positions.coronal <- c(2,1,3,4,3,1,1,3,3,1)		
	
#Reproduce their plot using specimen-level data
	
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
		
# For figure caption (species numbers)
cat('\n',paste( unique( paste0( specimen.info$unique.species.numbers , "- ", gsub("_", " ", specimen.info$Species)) ) , collapse= "; "))
				
				
#Get mean values for species
species_means <- specimen.info[,c("Species","Carapace.plastron_width","Length.height","Carapace_width_height","ecology")]
species_means <- subset(species_means,ecology!='fossil')


unique.species <- unique(species_means$Species)


#Get mean measurements 

CW_PW_ratio <- c() #Mean carapace width-plastron width ratio
coronal_doming_ratio <- c() #Mean coronal doming ratio
sagittal_doming_ratio <- c() #Mean sagittal doming ratio
ecologies <- c() #unique ecologies
species <- c() #unique species

for ( i in 1:nrow(species_means)){
  for ( j in 1:length(unique.species)){
    
    sp.temp <- unique.species[j]

      CW_PW_ratio[j] <- mean ( species_means[ grepl(sp.temp,species_means$Species) , "Carapace.plastron_width" ]   )
      coronal_doming_ratio[j] <- mean ( species_means[ grepl(sp.temp,species_means$Species) , "Carapace_width_height" ]   )
      sagittal_doming_ratio[j] <- mean ( species_means[ grepl(sp.temp,species_means$Species) , "Length.height" ]   )
      ecologies[j] <- unique ( species_means[ grepl(sp.temp,species_means$Species) , "ecology" ]   )
      species[j] <- unique ( species_means[ grepl(sp.temp,species_means$Species) , "Species" ]   )
            
    }
  }

names(CW_PW_ratio) <- names(coronal_doming_ratio) <- names(sagittal_doming_ratio) <- names(ecologies) <- names(species) <- unique.species

#bind them all together
species_means  <- data.frame(species,CW_PW_ratio, coronal_doming_ratio, sagittal_doming_ratio, ecologies)

#fossil measurements  
fossil_measurements <- subset(specimen.info,ecology=='fossil')
fossil_measurements <- fossil_measurements[,c('Species',"Carapace.plastron_width","Carapace_width_height","Length.height",'ecology')]
colnames(fossil_measurements) <- colnames(species_means)

#bind species mean values + fossil measurements
species_means_and_fossils <- rbind(species_means,fossil_measurements)
species_means_and_fossils$numbers <- as.numeric(as.factor(species_means_and_fossils$species))

#Get mean from two specimens of Basilemys variolosa
species_means_and_fossils[species_means_and_fossils$species=='Basilemys_variolosa',c(2,3,4)][2,] <-  apply(subset(species_means_and_fossils,species=="Basilemys_variolosa")[,c(2,3,4)],2,mean)
species_means_and_fossils <- species_means_and_fossils[-(nrow(species_means_and_fossils)-1),]

#Dataset for pfda analysis (includes extant species means, but for fossils only the 'true' measurements)
data_pfda <- species_means_and_fossils
data_pfda <- species_means_and_fossils[ !grepl('unknown',ignore.case = T,x=rownames(species_means_and_fossils)) & !grepl('gaffneyi',ignore.case = T,x=rownames(species_means_and_fossils)),]
rownames(data_pfda) <- data_pfda$species

data_pfda <- data_pfda[complete.cases(data_pfda),]


#colour schemes for points and within-point texts
ecology_col <- c( "dodgerblue3"  , "#D4145A", "tan")
num_col <- c("white" , "black" , "black")


#plot

pdf('sagittal_doming_left_coronal_doming_right_means.pdf', useDingbats = F, width = 18, height = 9)

#dev.new(width = 18, height = 9)
par(mfrow = c(1,2))

#Sagittal doming plot
plot(species_means_and_fossils$sagittal_doming_ratio ~ species_means_and_fossils$CW_PW_ratio, 
     pch = 21, bg = ecology_col[as.numeric(as.factor(species_means_and_fossils$ecologies))], 
     cex = 2.1, xlab = "Carapace width-plastron width ratio" ,
     ylab = "Carapace length-height ratio (sagittal doming)" )	

#Below line for text identifying specimens
text(species_means_and_fossils$sagittal_doming_ratio ~ species_means_and_fossils$CW_PW_ratio,
     col=num_col[as.numeric(as.factor(species_means_and_fossils$ecologies))], 
     lab = as.numeric(as.factor(species_means_and_fossils$species)), cex=0.5)
#Below line for text identifying fossils
text(species_means_and_fossils[species_means_and_fossils$ecologies=='fossil',c(2,4)],
     col='black', 
     lab = species_means_and_fossils[species_means_and_fossils$ecologies=='fossil','species'], cex=0.5,
     pos=1)
#Below line for text identifying selected extant species
text(species_means_and_fossils[c('Malacochersus_tornieri','Homopus_boulengeri','Terrapene_coahuila'),c(2,4)],
     col='black', 
     lab = c('Malacochersus_tornieri','Homopus_boulengeri','Terrapene_coahuila'), cex=0.5,
     pos=1)

legend( "topleft" , legend = c( "aquatic" , "terrestrial" , "fossil") , 
        pch = 21 , pt.bg = c( "dodgerblue3" , "tan" , "#D4145A") ,
        cex = 1.2 , pt.cex = 2.5 , bty = "n" )

#Coronoal doming plot. For this plot, we use the 'data pfda' object, as it contains only the species with 'true' measurements, i.e., with corrected ratios
plot(data_pfda$coronal_doming_ratio ~ data_pfda$CW_PW_ratio, 
     pch = 21, bg = ecology_col[as.numeric(as.factor(data_pfda$ecologies))], 
     cex = 2.1, xlab = "Carapace width-plastron width ratio" ,
     ylab = "Carapace width-height ratio (coronal doming)" )	

#Below line for text identifying specimens
text(data_pfda$coronal_doming_ratio ~ data_pfda$CW_PW_ratio,
     col=num_col[as.numeric(as.factor(data_pfda$ecologies))], 
     lab = data_pfda$numbers, cex=0.5)
#Below line for text identifying fossils
text(data_pfda[data_pfda$ecologies=='fossil',c(2,3)],
     col='black', 
     lab = data_pfda[data_pfda$ecologies=='fossil','species'], cex=0.5,
     pos=1)
#Below line for text identifying selected extant species
text(data_pfda[c('Malacochersus_tornieri','Homopus_boulengeri','Terrapene_coahuila'),c(2,3)],
     col='black', 
     lab = c('Malacochersus_tornieri','Homopus_boulengeri','Terrapene_coahuila'), cex=0.5,
     pos=1)

legend( "topleft" , legend = c( "aquatic" , "terrestrial" , "fossil") , 
        pch = 21 , pt.bg = c( "dodgerblue3" , "tan" , "#D4145A") ,
        cex = 1.2 , pt.cex = 2.5 , bty = "n" )


dev.off()


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
extant_doming_data <- data_pfda[rownames(data_pfda) %in% trees.new[[1]]$tip.label ,]
extinct_doming_data <- data_pfda[!rownames(data_pfda) %in% rownames(extant_doming_data) & data_pfda$ecologies=="fossil" ,]

#For pfda, we only consider the values of the AMNH speciemn of Basilemys variolosa (AMNH 5448), as it appears less dorsoventrally flattened
extinct_doming_data['Basilemys_variolosa',2:4] <- fossil_measurements['Basilemys_variolosa_AMNH_5448',2:4]

#Not include 'Basilemys sp' for further pfda analysis
extinct_doming_data <- subset(extinct_doming_data,species!='Basilemys_sp')

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
  
  
  tree.temp <- bind.tip(tree.temp,tip.label = "Proganochelys_quenstedti",
                        edge.length = ifelse(tree.temp$root.time>220,
                                             tree.temp$root.time-201, 15) ,
                        
                        where=(Ntip(tree.temp)+1), 
                        position= 1
                          )
  
  tree.temp$root.time <- max(diag(vcv(tree.temp)))
  
  tree.temp$root.edge <- 1
  
  tree.temp <- bind.tip(tree.temp,tip.label = "Proterochersis_robusta",
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
  
  
  tree.temp <- bind.tip(tree.temp,tip.label = "Basilemys_variolosa",
                        edge.length = 4,
                        where=which(tree.temp$tip.label == 'Basilemys_morrinensis'),
                        position= 10)
  
  scaled_trees_doming[[i]] <- tree.temp
  
  setTxtProgressBar(txtProgressBar(0,length(trees.new),style = 3),i)
  
}

class(scaled_trees_doming) <- "multiPhylo"

dev.new()
plot(scaled_trees_doming[[1]], cex = 0.5)
dev.off()

#pFDA with 100 iterations (coronal doming- this study)

reps =  length(scaled_trees_doming)
pFDA.doming <- list()
lambda.doming <- list()

for ( i in 1:reps){
  
  doming_data.temp <- data_pfda[scaled_trees_doming[[i]]$tip.label,]
  XA.temp <- doming_data.temp[,c(2,3)] 
  treA.temp <- scaled_trees_doming[[i]]
  #if(!is.binary.tree(treA)) treA <- multi2di(treA, random = TRUE) 
  #is.ultrametric(treA)
  
  ddA.temp <- XA.temp[treA.temp$tip.label,]
  taxaA.temp <- rownames(ddA.temp)
  gA.temp <- as.factor(doming_data.temp$ecologies)
  
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
                        function(x) mean(rownames(dd.temp)[which(doming_data.temp$ecologies=="aquatic")] %in% names(x)[which(x=="aquatic")])  )))[c(1,3,6)]

# % correct "terrestrial" classifications
summary(unlist(lapply ( doming_predictions_extant ,
                        function(x) mean(rownames(dd.temp)[which(doming_data.temp$ecologies=="terrestrial")] %in% names(x)[which(x=="terrestrial")])  )))[c(1,3,6)]


overall_sucess <- summary( unlist(lapply ( pFDA.doming, 
                           function(x) as.numeric(attributes(print(x$confusion))$error) )))

overall_sucess <- 1-overall_sucess[c(1,3,6)]
  names(overall_sucess) <- rev(names(overall_sucess))
overall_sucess


#####

#pFDA with 100 iterations (sagittal doming- original study)

reps =  length(scaled_trees_doming)
pFDA.sagittal <- list()
lambda.sagittal <- list()

for ( i in 1:reps){
  
  doming_data.temp <- data_pfda[scaled_trees_doming[[i]]$tip.label,]
  XA.temp <- doming_data.temp[,c(2,4)] 
  treA.temp <- scaled_trees_doming[[i]]
  #if(!is.binary.tree(treA)) treA <- multi2di(treA, random = TRUE) 
  #is.ultrametric(treA)
  
  ddA.temp <- XA.temp[treA.temp$tip.label,]
  taxaA.temp <- rownames(ddA.temp)
  gA.temp <- as.factor(doming_data.temp$ecologies)
  
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
  
  lambda.sagittal[[i]] <- lambda.temp
  
  #pfda
  optl.temp <- ol1.temp$optlambda[1,1]
  pFDA.temp <- phylo.fda.pred(XA.temp,gA.temp,taxaA.temp,
                              treA.temp,
                              testtaxan.temp,
                              val=optl.temp,eqprior = T)
  
  pFDA.sagittal[[i]] <- pFDA.temp
  
  setTxtProgressBar(txtProgressBar(0,reps,style = 3),i)
  
}


#Predictions
sagittal_predictions <- lapply(pFDA.sagittal, 
                             function(x) predict(x, newdata=x$DATAtest,
                                                 type="posterior"))
sagittal_predictions <- lapply ( sagittal_predictions , 
                               function(x) {rownames(x) <- testtaxa.temp ; x})

#Predicted medians
sagittal_predictions_median <- round( rbind( apply( do.call(rbind, lapply ( sagittal_predictions ,  function(x) x[1,] )) , 2 , function(x) summary(x)[c(1,3,6)] ) , 
                                           apply( do.call(rbind, lapply ( sagittal_predictions ,  function(x) x[2,] )) , 2 ,function(x) summary(x)[c(1,3,6)]),
                                           apply( do.call(rbind, lapply ( sagittal_predictions ,  function(x) x[3,] )) , 2 ,function(x) summary(x)[c(1,3,6)]),
                                           apply( do.call(rbind, lapply ( sagittal_predictions ,  function(x) x[4,] )) , 2 ,function(x) summary(x)[c(1,3,6)]),
                                           apply( do.call(rbind, lapply ( sagittal_predictions ,  function(x) x[5,] )) , 2 ,function(x) summary(x)[c(1,3,6)])) , 3)

rownames(sagittal_predictions_median) <- paste0(rep(sort(rownames(extinct_doming_data)),each=3) ,'_', c('min','median','max'))
sagittal_predictions_median

# number of trees in which each taxon was predicted each ecology

sapply(1:nrow(extinct_doming_data) , 
       function(sp) table(unlist(lapply ( pFDA.sagittal , function(x) x$testprediction[sp,]))))

##success rates for extant
sagittal_predictions_extant <- lapply ( pFDA.sagittal , function(x) predict(x, type="class"))
sagittal_predictions_extant <- lapply(sagittal_predictions_extant , function(x){names(x) <- trainingtaxa.temp; x})

# % correct "aquatic" classifications
summary(unlist(lapply ( sagittal_predictions_extant ,
                        function(x) mean(rownames(dd.temp)[which(doming_data.temp$ecologies=="aquatic")] %in% names(x)[which(x=="aquatic")])  )))[c(1,3,6)]

# % correct "terrestrial" classifications
summary(unlist(lapply ( sagittal_predictions_extant ,
                        function(x) mean(rownames(dd.temp)[which(doming_data.temp$ecologies=="terrestrial")] %in% names(x)[which(x=="terrestrial")])  )))[c(1,3,6)]


overall_sucess <- summary( unlist(lapply ( pFDA.sagittal, 
                                           function(x) as.numeric(attributes(print(x$confusion))$error) )))

overall_sucess <- 1-overall_sucess[c(1,3,6)]
  names(overall_sucess) <- rev(names(overall_sucess))
overall_sucess


#Post-review
#Running analyses without 'weird' (many air quotes here) taxa such as Malacochersus, Homopus boulengeri and Terrapene coahuila

to_drop <- c("Malacochersus_tornieri",'Homopus_boulengeri','Terrapene_coahuila')

#pFDA with 100 iterations (coronal doming- this study)

reps =  length(scaled_trees_doming)
pFDA.doming_withoutOutliers <- list()
lambda.doming_withoutOutliers <- list()

for ( i in 1:reps){
  
  doming_data.temp <- data_pfda[scaled_trees_doming[[i]]$tip.label,]
    doming_data.temp <- doming_data.temp[!rownames(doming_data.temp) %in% to_drop,]
  XA.temp <- doming_data.temp[,c(2,3)] 
  treA.temp <- keep.tip(scaled_trees_doming[[i]],rownames(doming_data.temp))
  #if(!is.binary.tree(treA)) treA <- multi2di(treA, random = TRUE) 
  #is.ultrametric(treA)
  
  ddA.temp <- XA.temp[treA.temp$tip.label,]
  taxaA.temp <- rownames(ddA.temp)
  gA.temp <- as.factor(doming_data.temp$ecologies)
  
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
  
  lambda.doming_withoutOutliers[[i]] <- lambda.temp
  
  #pfda
  optl.temp <- ol1.temp$optlambda[1,1]
  pFDA.temp <- phylo.fda.pred(XA.temp,gA.temp,taxaA.temp,
                              treA.temp,
                              testtaxan.temp,
                              val=optl.temp,eqprior = T)
  
  pFDA.doming_withoutOutliers[[i]] <- pFDA.temp
  
  setTxtProgressBar(txtProgressBar(0,reps,style = 3),i)
  
}


#Predictions
doming_predictions <- lapply(pFDA.doming_withoutOutliers, 
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
       function(sp) table(unlist(lapply ( pFDA.doming_withoutOutliers , function(x) x$testprediction[sp,]))))

##success rates for extant
doming_predictions_extant <- lapply ( pFDA.doming_withoutOutliers , function(x) predict(x, type="class"))
doming_predictions_extant <- lapply(doming_predictions_extant , function(x){names(x) <- trainingtaxa.temp; x})

# % correct "aquatic" classifications
summary(unlist(lapply ( doming_predictions_extant ,
                        function(x) mean(rownames(dd.temp)[which(doming_data.temp$ecologies=="aquatic")] %in% names(x)[which(x=="aquatic")])  )))[c(1,3,6)]

# % correct "terrestrial" classifications
summary(unlist(lapply ( doming_predictions_extant ,
                        function(x) mean(rownames(dd.temp)[which(doming_data.temp$ecologies=="terrestrial")] %in% names(x)[which(x=="terrestrial")])  )))[c(1,3,6)]


overall_sucess <- summary( unlist(lapply ( pFDA.doming_withoutOutliers, 
                                           function(x) as.numeric(attributes(print(x$confusion))$error) )))

overall_sucess <- 1-overall_sucess[c(1,3,6)]
overall_sucess




#pFDA with 100 iterations (sagittal doming- original study)

reps =  length(scaled_trees_doming)
pFDA.sagittal_withoutOutliers <- list()
lambda.sagittal_withoutOutliers <- list()

for ( i in 1:reps){
  
  doming_data.temp <- data_pfda[scaled_trees_doming[[i]]$tip.label,]
    doming_data.temp <- doming_data.temp[!rownames(doming_data.temp) %in% to_drop,]
  XA.temp <- doming_data.temp[,c(2,4)] 
  treA.temp <- keep.tip(scaled_trees_doming[[i]],rownames(doming_data.temp))
  #if(!is.binary.tree(treA)) treA <- multi2di(treA, random = TRUE) 
  #is.ultrametric(treA)
  
  ddA.temp <- XA.temp[treA.temp$tip.label,]
  taxaA.temp <- rownames(ddA.temp)
  gA.temp <- as.factor(doming_data.temp$ecologies)
  
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
  
  lambda.sagittal_withoutOutliers[[i]] <- lambda.temp
  
  #pfda
  optl.temp <- ol1.temp$optlambda[1,1]
  pFDA.temp <- phylo.fda.pred(XA.temp,gA.temp,taxaA.temp,
                              treA.temp,
                              testtaxan.temp,
                              val=optl.temp,eqprior = T)
  
  pFDA.sagittal_withoutOutliers[[i]] <- pFDA.temp
  
  setTxtProgressBar(txtProgressBar(0,reps,style = 3),i)
  
}


#Predictions
sagittal_predictions <- lapply(pFDA.sagittal_withoutOutliers, 
                               function(x) predict(x, newdata=x$DATAtest,
                                                   type="posterior"))
sagittal_predictions <- lapply ( sagittal_predictions , 
                                 function(x) {rownames(x) <- testtaxa.temp ; x})

#Predicted medians
sagittal_predictions_median <- round( rbind( apply( do.call(rbind, lapply ( sagittal_predictions ,  function(x) x[1,] )) , 2 , function(x) summary(x)[c(1,3,6)] ) , 
                                             apply( do.call(rbind, lapply ( sagittal_predictions ,  function(x) x[2,] )) , 2 ,function(x) summary(x)[c(1,3,6)]),
                                             apply( do.call(rbind, lapply ( sagittal_predictions ,  function(x) x[3,] )) , 2 ,function(x) summary(x)[c(1,3,6)]),
                                             apply( do.call(rbind, lapply ( sagittal_predictions ,  function(x) x[4,] )) , 2 ,function(x) summary(x)[c(1,3,6)]),
                                             apply( do.call(rbind, lapply ( sagittal_predictions ,  function(x) x[5,] )) , 2 ,function(x) summary(x)[c(1,3,6)])) , 3)

rownames(sagittal_predictions_median) <- paste0(rep(sort(rownames(extinct_doming_data)),each=3) ,'_', c('min','median','max'))
sagittal_predictions_median

# number of trees in which each taxon was predicted each ecology

sapply(1:nrow(extinct_doming_data) , 
       function(sp) table(unlist(lapply ( pFDA.sagittal_withoutOutliers , function(x) x$testprediction[sp,]))))

##success rates for extant
sagittal_predictions_extant <- lapply ( pFDA.sagittal_withoutOutliers , function(x) predict(x, type="class"))
sagittal_predictions_extant <- lapply(sagittal_predictions_extant , function(x){names(x) <- trainingtaxa.temp; x})

# % correct "aquatic" classifications
summary(unlist(lapply ( sagittal_predictions_extant ,
                        function(x) mean(rownames(dd.temp)[which(doming_data.temp$ecologies=="aquatic")] %in% names(x)[which(x=="aquatic")])  )))[c(1,3,6)]

# % correct "terrestrial" classifications
summary(unlist(lapply ( sagittal_predictions_extant ,
                        function(x) mean(rownames(dd.temp)[which(doming_data.temp$ecologies=="terrestrial")] %in% names(x)[which(x=="terrestrial")])  )))[c(1,3,6)]


overall_sucess <- summary( unlist(lapply ( pFDA.sagittal_withoutOutliers, 
                                           function(x) as.numeric(attributes(print(x$confusion))$error) )))

overall_sucess <- 1-overall_sucess[c(1,3,6)]
overall_sucess


