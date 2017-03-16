
### Visualization tool for the FREM model, this version is based on the FREM model for the Cohorts data set only
### Directory "supportfunctions" contains helper functions for the app
### Directory "FREMmodel" contains NONMEM model code and files for parameter estimates
### Written by Joakim Nyberg and Niclas Jonsson, Pharmetheus for the Gates project, 2017-01-25

library(shiny)
library(ggplot2)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)

source('./supportfunctions/getExt.R')
source('./supportfunctions/calcFFEM.R')
source('./supportfunctions/calcHAZVector.R')
source('./supportfunctions/getCovNames.R')
source('./supportfunctions/ind_estimates.R')
source('./supportfunctions/ind_likelihood.R')

meansfile<-"presetmeans_df.csv"

covnames<-getCovNames(modFile="./FREMmodel/run28.mod",keepComment=FALSE) #Get the covariate names from the FREM model
covnamescogn<-getCovNames(modFile="./FREMmodel/run28_cogn_haz_onlyiqchilds.mod",keepComment=FALSE) #Get the covariate names from the FREM model

#### Color definitions
plotBGColor<-"#E8E4DA" #Background color for all plots
plotGridLinesColor<-"#1D1D1B"
plotTitleColor<-"#9C252E"
plotAxisLabelColor<-"#1D1D1B" #The color of the text on the x and y-axis (and the tick text color)
cColorOrange<-"#F39C12" #The orange color used for some dotted lines etc.
cColorBlue <- "#3186AC" #The blue color used for some lines etc.
cColorLightBlue <- "#78BCDA"
cColorWhite <- "#FFFFFF"
cColorLightRed<-"#E5676F"
strStuntedLineType<-"longdash"


theme_set(theme_bw(base_size=22))

theme_update(text = element_text(family = "Helvetica"),
             panel.background = element_rect(fill = "transparent",colour = NA),
             plot.background  = element_rect(fill = plotBGColor,colour = plotBGColor),
             plot.margin = unit(c(2, 0, .5, 0.05), "cm"),
             panel.border      = element_rect(fill = NA, colour = plotGridLinesColor,size=1),
             panel.grid.major = element_line(colour = plotGridLinesColor,size=.15),
             panel.ontop = TRUE,
             plot.title = element_text(colour = plotTitleColor,hjust=0, face="bold",margin = margin(b = 40)),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_line(size=1.5),
             axis.ticks.length = unit(0.5, "cm"),
             axis.text = element_text(color = plotAxisLabelColor),
             axis.title = element_text(color = plotAxisLabelColor, face = "bold"),
             panel.grid.minor = element_blank(),
             legend.background = element_rect(fill=plotBGColor, colour = plotBGColor),
             legend.position = c(1,1),
             legend.justification = c("right", "bottom"),
             legend.title = element_text(margin = margin(b = 30)), #face = "bold"
             legend.key = element_rect(fill = plotBGColor, colour = plotBGColor),
             legend.key.width = unit(2, "cm"),
             legend.key.size = unit(1.5, 'lines'),
             strip.background=element_rect(fill = plotBGColor,colour = NA),
             strip.text=element_text(color = plotAxisLabelColor, face="bold")
             )

t1=0 #Min time
t2=15 #Max time
resolution=1000 #Number of model evaluations in x-direction (age-dimension)
numIIVSamples<-5000 #Number of IIV samples (per parameter/dimension)
bUseSampling<-TRUE #If sampling is to be used for IIV instead of propagation of error, slower but more accurate
strSeed<-124 #The seed for sampling for reproduciability
strMissing<-"Missing"
noBaseThetas<-6
noCovThetas<-52
numcolumns<-4 #Number of columns in input panel
ebeuncertainty<-95 #The level om ebe uncertainty to show
popvariance<-95 #The prediction interval to show for population
lineSize<-1.5 #The line size of the plot
lowlimitHAZ<- -4 #The upper and lower HAZ limits for the figures
highlimitHAZ<- 4

if (bUseSampling) {
  set.seed(strSeed)
  iivsamples<-matrix(0,ncol=noBaseThetas,nrow=numIIVSamples)
  for (i in 1:noBaseThetas) {
    iivsamples[,i]<-rnorm(numIIVSamples)
  }
}

tim=seq(from = t1,to = t2,length.out = resolution) #Make a time input vector
dfext<-subset(getExt(extFile = './FREMmodel/run28.ext'),ITERATION=="-1000000000") #Read in parameter values
dfextcogn<-subset(getExt(extFile = './FREMmodel/run28_cogn_haz_onlyiqchilds.ext'),ITERATION=="-1000000000") #Read in parameter values


thbasevector<-as.numeric(dfext[2:(noBaseThetas+1)])
df_thm <- as.numeric(dfext[,(noBaseThetas+2):(noBaseThetas+1+noCovThetas)]) #Get the mean estimate from the FREM covariates
sig<-as.numeric(dfext[noBaseThetas+2+noCovThetas]) #Get the additive residual error (variance)
typical<-calcHAZVector(age = tim,basethetas = thbasevector)

###Cognition
noCovThetasCogn<-70

thbasevectorcogn<-as.numeric(dfextcogn[2:(noBaseThetas+1)])
df_thmcogn <- as.numeric(dfextcogn[,(noBaseThetas+2):(noBaseThetas+1+noCovThetasCogn)]) #Get the mean estimate from the FREM covariates
sigcogn<-as.numeric(dfextcogn[noBaseThetas+2+noCovThetasCogn]) #Get the additive residual error (variance)

#noCovThetas=length(covNames)
noParCov<-6 #Assume no other parameters, only omega block
iNumFREMOM<-(noCovThetasCogn+noParCov)*(noCovThetasCogn+noParCov+1)/2

df_om_cogn  <- as.numeric(dfextcogn[,(noBaseThetas+5+noCovThetasCogn):(noBaseThetas+4+noCovThetasCogn+iNumFREMOM)])

num_om_cogn <- -1/2+sqrt(1/4+2*iNumFREMOM)
om_matrix_cogn          <- as.numeric(df_om_cogn)
OM_cogn                             <- matrix(0, nrow=num_om_cogn, ncol=num_om_cogn) #Define an empty matrix
OM_cogn[upper.tri(OM_cogn,diag = TRUE)]  <- om_matrix_cogn #Assign upper triangular + diag
tOM                            <- t(OM_cogn) #Get a transposed matrix
OM_cogn[lower.tri(OM_cogn,diag = FALSE)] <- tOM[lower.tri(tOM,diag = FALSE)] #Assign the lower triangular except diag

df_thm_full<-df_thmcogn #Save for use later on


### Remove the OM corresponding to parameters that we're not interested in, i.e. the original HAZ params
OM_cogn<-OM_cogn[-(1:noParCov),-(1:noParCov)]
OM_FULL<-OM_cogn #Save for use later on

cParamIndex<-c(53,54,55,56) #IQ11, IQ15, ENG13, MAT13
#cParamIndex<-c(53) #IQ11

#OM_cogn<-OM_cogn[-c(54,55,56),-c(54,55,56)]

OM_PAR     <- OM_cogn[cParamIndex,cParamIndex] #The parameter covariance matrix

OM_COV     <- OM_cogn[-cParamIndex,-cParamIndex] #The covariates covariance matrix
OM_PAR_COV <- OM_cogn[cParamIndex,-cParamIndex] #The covariance between covariates and parameters matrix
OM_PAR_COV <-as.matrix(OM_PAR_COV)
if (ncol(OM_PAR_COV)==1) OM_PAR_COV<-t(OM_PAR_COV)

inv       <- solve(OM_COV)
COEFF     <- OM_PAR_COV%*%inv #The parameter-covariate coefficients 
COEFF_VAR_COGN_ALL_COV <- OM_PAR-OM_PAR_COV%*%inv%*%t(OM_PAR_COV) #The cognitive iiv variances given all covariates (except cognitive covariates)
COEFF_VAR_COGN_NO_COV <- OM_PAR #The cognitive iiv variances given no covariates
COGN_MEAN<-df_thm_full[cParamIndex]


df_means<-read.csv(file=meansfile,stringsAsFactors = FALSE) #Read in the analysis dataset to calculate covariate means per SITEID after removing missing covariate values

tmp<-c()
tmp$data<-typical
tmp$color<-plotGridLinesColor
tmp$legend<-"Missing all"
tmp$linetype<-"filled"

plotList<-list(tmp)

indvalue<-c()
indvalue$data<-typical
indvalue$color<-cColorBlue
indvalue$legend<-"Using known"
indvalue$linetype="filled"

tmp<-calcFFEM(dfext = dfext,noBaseThetas = noBaseThetas,noCovThetas = noCovThetas,noParCov = noBaseThetas,availCov = NULL,quiet = TRUE)


#Type = 1 is Numeric input with default value of defvalue, and values between minval-maxval, Type=2 is radiobutton input with possible values of possiblevalues which is a vector of possible values
covariatedynamic<-function(strDisplayName="Covariate",iType=1,defvalue=NA,minval=0,maxval=100,strUnit="",possibleValues=NULL,codedValues=NULL,treatAsContinuous=FALSE){
  
  covlistElement<-list()
  covlistElement[["DisplayName"]]<-strDisplayName
  covlistElement[["Type"]]<-iType
  covlistElement[["Unit"]]<-strUnit
  if (iType==1) { #This is a numeric input
    covlistElement[["Default"]]<-defvalue
    covlistElement[["Min"]]<-minval
    covlistElement[["Max"]]<-maxval
  }
  if (iType==2 || iType==3) { #This is a radiobutton input or a combobox input
    covlistElement[["Values"]]<-c(possibleValues,strMissing)
    covlistElement[["CodedValues"]]<-codedValues #The coded values of the Values, e.g. Values=c("Male","Female","Missing"), CodedValues=c(1,2)
    covlistElement[["Default"]]<-strMissing
    covlistElement[["Continuous"]]<-treatAsContinuous #Set to true if this variable shoudl be treated as continuous in the model (but discrete in the input)
  }
  return(covlistElement)
}
#### Statically define the covariates and their values, types and limits
covlist<-list(    )
covlist[["BIRTHLEN"]]<-covariatedynamic(strDisplayName = "Birth length",iType = 1,minval = 30,maxval = 65,strUnit = "cm")
covlist[["BIRTHWT"]]<-covariatedynamic(strDisplayName = "Birth weight",iType = 1,minval = 0.5,maxval = 7,strUnit = "kg")
covlist[["CHLDADLT"]]<-covariatedynamic(strDisplayName = "Ratio of children to adults",iType = 1,minval = 0,maxval = 10,strUnit = "")
covlist[["CHLDDR"]]<-covariatedynamic(strDisplayName = "Child dependency ratio",iType = 1,minval = 0,maxval = 30,strUnit = "")
covlist[["CRWDINDX"]]<-covariatedynamic(strDisplayName = "Crowding index",iType = 1,minval = 0,maxval = 10,strUnit = "")
covlist[["DLVLOCN"]]<-covariatedynamic(strDisplayName = "Location of delivery",iType = 2,possibleValues=c("Home","Hospital"),codedValues=c(0,4))
covlist[["DURBRST"]]<-covariatedynamic(strDisplayName = "Breastfeeding duration",iType = 1,minval = 0,maxval = 100,strUnit = "months")
covlist[["FAGE"]]<-covariatedynamic(strDisplayName = "Fathers age at birth",iType = 1,minval = 12,maxval = 100,strUnit = "years")
covlist[["FEDUCYRS"]]<-covariatedynamic(strDisplayName = "Fathers education",iType = 1,minval = 0,maxval = 100,strUnit = "years")
covlist[["FEEDINGN"]]<-covariatedynamic(strDisplayName = "Feeding practice",iType = 2,possibleValues=c("Not breast feeding","Breast feeding"),codedValues=c(0,1))
covlist[["FHTCM"]]<-covariatedynamic(strDisplayName = "Fathers height",iType = 1,minval = 120,maxval = 230,strUnit = "cm")
covlist[["FSMAGE"]]<-covariatedynamic(strDisplayName = "Mothers age at time of 1st delivery",iType = 1,minval = 12,maxval = 60,strUnit = "years")
covlist[["GAGEBRTH"]]<-covariatedynamic(strDisplayName = "Gestational age at birth",iType = 1,minval = 100,maxval = 340,strUnit = "days")
covlist[["H2OAVAIL"]]<-covariatedynamic(strDisplayName = "Water availability (access to safe water)",iType = 2,possibleValues=c("Worst","Moderate access","Good access"),codedValues=c(0,1,2))
covlist[["HCACCESS"]]<-covariatedynamic(strDisplayName = "Access to healthcare",iType = 2,possibleValues=c("Poor access","Good access"),codedValues=c(0,1))
covlist[["HCUTILIZ"]]<-covariatedynamic(strDisplayName = "Health service utilization",iType = 2,possibleValues=c("Low use","Intermediate use","Highest use"),codedValues=c(0,1,2))
covlist[["HOMETYPE"]]<-covariatedynamic(strDisplayName = "Home type",iType = 2,possibleValues=c("Thatched hut","Masonry build","Block of flats","Bungalow"),codedValues=c(1,2,3,4))
covlist[["INCTOT"]]<-covariatedynamic(strDisplayName = "Income quartile (per site)",iType = 2,possibleValues=c("Q1","Q2","Q3","Q4"),codedValues=c(1,2,3,4),treatAsContinuous=TRUE)
covlist[["MAGE"]]<-covariatedynamic(strDisplayName = "Mothers age at birth",iType = 1,minval = 12,maxval = 100,strUnit = "years")
covlist[["MATEMPQT"]]<-covariatedynamic(strDisplayName = "Maternal empowerment (quartile)",iType = 2,possibleValues=c("Q1","Q2","Q3","Q4"),codedValues=c(1,2,3,4),treatAsContinuous=TRUE)
covlist[["MEDUCYRS"]]<-covariatedynamic(strDisplayName = "Mothers education",iType = 1,minval = 0,maxval = 100,strUnit = "years")
covlist[["MHTCM"]]<-covariatedynamic(strDisplayName = "Mothers height",iType = 1,minval = 100,maxval = 230,strUnit = "cm")
covlist[["MMARITN"]]<-covariatedynamic(strDisplayName = "Mothers marital status ",iType = 2,possibleValues=c("Married","Single"),codedValues=c(1,6))
covlist[["NADULTF"]]<-covariatedynamic(strDisplayName = "Number of adult females",iType = 1,minval = 0,maxval = 20,strUnit = "")
covlist[["NADULTM"]]<-covariatedynamic(strDisplayName = "Number of adult males",iType = 1,minval = 0,maxval = 20,strUnit = "")
covlist[["NCHLD"]]<-covariatedynamic(strDisplayName = "Number of children in the home",iType = 1,minval = 0,maxval = 40,strUnit = "")
covlist[["NFCHILD"]]<-covariatedynamic(strDisplayName = "Maternal num of female children",iType = 1,minval = 0,maxval = 40,strUnit = "")
covlist[["NMCHILD"]]<-covariatedynamic(strDisplayName = "Maternal num of male children",iType = 1,minval = 0,maxval = 40,strUnit = "")
covlist[["NPERSON"]]<-covariatedynamic(strDisplayName = "Number of persons in house",iType = 1,minval = 0,maxval = 40,strUnit = "")
covlist[["NROOMS"]]<-covariatedynamic(strDisplayName = "Number of rooms in house",iType = 1,minval = 1,maxval = 20,strUnit = "")
covlist[["PRTY"]]<-covariatedynamic(strDisplayName = "Maternal parity",iType = 2,possibleValues=c("First born","Second born","Third born","Born fourth or later"),codedValues=c(1,2,3,4),treatAsContinuous=TRUE)
covlist[["RACEN"]]<-covariatedynamic(strDisplayName = "Race",iType = 2,possibleValues=c("American Indian or Alaska Native","Asian","Black","White","Mixed race","Other race"),codedValues=c(1,2,3,5,6,7))
covlist[["SANITATN"]]<-covariatedynamic(strDisplayName = "Sanitary facility type",iType = 2,possibleValues=c("No toilet","Some excretal removal","Flush"),codedValues=c(0,1,2))
covlist[["SESN"]]<-covariatedynamic(strDisplayName = "Socioeconomic status of parent",iType = 2,possibleValues=c("Low","Lower-middle","Middle","Upper-middle","Upper"),codedValues=c(25,38,50,63,75))
covlist[["SEXN"]]<-covariatedynamic(strDisplayName = "Gender of child",iType = 2,possibleValues=c("Male","Female"),codedValues=c(1,2))
covlist[["SITEID"]]<-covariatedynamic(strDisplayName = "Site",iType = 3,possibleValues=c("Brazil","Guatemala","India","Philippines","South Africa"),codedValues=c(1,2,3,4,5))
covlist[["SMOKED"]]<-covariatedynamic(strDisplayName = "Smoked during pregnancy",iType = 2,possibleValues=c("No","Yes"),codedValues=c(0,1))



#Add estimated mean values for the continuous covariates (and the ones with 2 categories)
for (i in 1:length(covlist)) {
  if (length(df_thm[which(covnames$covNames==names(covlist[i]))])>0) covlist[[i]]$Mean<-df_thm[which(covnames$covNames==names(covlist[i]))]
}


numinput<-length(covlist)

numinputpercol<-ceiling(numinput/numcolumns) #Defien the number of input covariate items per column in the input display

#Makes a list element of predefed values, with the covariates in covvector predefined. Either predefined using meanvalues or displayvalues.
#If meanvalues are entered these are used to map to the closest (exact for continuous or rounded for categorical) displayvalues by using covlist
#If only displayvalues are entered, theres are used to set the meanvalues correctly instead
predefelement<-function(strDisplayName="Predefine",covvector,meanvalues=NULL,displayvalues=NULL,covlist){
  
  predefElement<-list()
  predefElement[["DisplayName"]]<-strDisplayName
  predefElement[["CovariateVector"]]<-covvector
  
  if (is.null(displayvalues)) { #The display values needs to be calculated
    displayvalues<-rep(strMissing,length(covvector))
    for (i in 1:length(covvector)) {
      covelement<-covlist[[covvector[i]]] #Get the covariate definition
      if (covelement$Type==1) #Cont covariate, set displayvalue to string version of mean
        displayvalues[i]<-paste0(round(meanvalues[i],3))
      if (covelement$Type==2 || covelement$Type==3) {#If it is a categorical variable
        displayvalues[i]<-covelement$Values[which(min(abs(covelement$CodedValues-meanvalues[i]))==abs(covelement$CodedValues-meanvalues[i]))]
      }
    }
  }
  predefElement[["MeanValues"]]<-meanvalues
  predefElement[["DisplayValues"]]<-displayvalues
  
  return(predefElement)
}


#Make a list with pre-defined values that could be used to set different scenarios
predeflist<-list()
for (siteid in unique(df_means$SITEID)) { #Add all SITEs (using their mean values) as predefined scenarios
  strSite<-c("Brazil","Guatemala","India","Philippines","South Africa")[siteid]
  predeflist[[strSite]]<-predefelement(strSite,c(df_means$key[df_means$SITEID==siteid],"SITEID"),c(df_means$mean[df_means$SITEID==siteid],siteid),displayvalues=NULL,covlist)
}

### Add some other predefine values
predeflist[["Risk child"]]<-predefelement("Risk child",c("SITEID","BIRTHLEN","BIRTHWT","H2OAVAIL","HOMETYPE"),c(2,45,2.8,0,1),NULL,covlist)


#The user interface function

ui<-fluidPage(theme = "bootstrap.css",
  
    title = "Supermodel approach",
    tabsetPanel(
      tabPanel("Expected HAZ", 
    
      plotOutput(outputId = "modelcurve"),
       fluidRow(column(2,h5("Lock curve"),align="bottom",actionButton("lockButton",label="Lock")),
               column(2,textInput("inputLegend",label=h5("Legend"),value = "Legend")),
               column(2,textInput("inputColor",label=h5("Color"),value = "#00FF00")),
               column(2,h5("Reset curve"),actionButton("resetButton","Reset")),
               column(2,uiOutput(paste0("input_ui_select")))),
    hr(),
    p("Individual covariate values:"),
    wellPanel(
    fluidRow({
      lapply(1:numcolumns, function(i) {
          return(column(3,uiOutput(paste0("input_ui_",i))))
        })
      }),style = "overflow-y:scroll; max-height: 450px"
    )
      ),
    tabPanel("Population HAZ",
             plotOutput(outputId = "modelcurvepop"),
             h4("All figures are based on the covariates at the Expected HAZ tab and are using individual variability around the expected haz curve (white area)."),
             plotOutput(outputId = "stuntedcurve"),
             plotOutput(outputId = "nadirdist")
             ),
    tabPanel("Individual HAZ",
             plotOutput(outputId = "modelcurveind"),
             h4(paste0("The white shaded area represent the ",ebeuncertainty,"% CI around the individual haz curve. The vertical line represent the next most informative observation time (based on D-optimal design).")),
             wellPanel(
               fluidRow(column(2,h5("Add observation"),actionButton("AddObsButton","Add"),tags$style(type='text/css', "#AddObsButton { vertical-align: middle; }")),
                        column(2,align="center",textInput("inputAge",label=h5("Age (years)"),value = "")),
                        column(2,align="center",textInput("inputHaz",label=h5("Haz"),value = "")),
                        column(3,h5("Reset observations"),actionButton("resetButtonData","Reset"))),
               fluidRow(
               column(12,
                      tableOutput('tableObs')
               ))
             )),
    
tabPanel("Cognition",
         plotOutput(outputId = "cognitioncurve"),
         checkboxGroupInput("cognitionOptions", 
                            label = h3("Predictions based on:"), 
                            choices = list("No covariates and no HAZ (A)" = 1, 
                                           "No covariates and expected HAZ (B)" = 2, 
                                           "Specified covariates and expected HAZ (C)" = 3,
                                           "Specified covariates and individual HAZ (D)" = 4),
                            selected = c(1,2)),
         plotOutput(outputId = "explainedcognvar"),
         radioButtons(inputId="cognvaroptions", "Explained covariate variability based on:",
                     c("Specified covariates and expected HAZ (C)" = "both",
                       "Specified covariates" = "onlyCov",
                       "Expected HAZ" = "onlyHAZ"))
    )
))


#Calculate uncertainty of transformed variables using propagation of uncertainty (delta-method)
th_deltarule<- function(th_vector,th_covmatrix,transform_fun,transform_derivFun=NULL,bIncludeCov=TRUE,...) {
  
  th_new<-transform_fun(th_vector,...) #Calculate the transformation
  
  if (is.null(transform_derivFun)) { #Calculate the derivatives by numeric differentiation
    library(numDeriv)   
    th_deriv<-grad(transform_fun,th_vector,method="Richardson", side=NULL, method.args=list(), ...) 
  }
  else {
    th_deriv<-transform_derivFun(th_vector) #Get the derivatives of the transformation w.r.t. theta
  }
  
  new_var<-0 #Initialize the variance of the transformed function
  
  for (i in 1:length(th_vector))
    for (j in 1:length(th_vector)) {
      if ((!bIncludeCov && i==j) || bIncludeCov) new_var<-new_var+th_deriv[i]*th_deriv[j]*th_covmatrix[i,j]      
    }
  
  return (c(th_new,new_var)) #Return a vector with the tranformed parameter value and the transformed parameter variance
}

wrapper_func <-function(ebe_vector,thetas,age){
  calcHAZVector(age,thetas[1:6],thetas[7:12],ebe_vector)
}

wrapper_func_tnadir <- function(ebe_vector,thetas) {
  
  ## Get the model based  time of nadir
  tnadir<-optimize(calcHAZVector,interval=c(t1,t2),thetas[1:6],covthetas=thetas[7:12],etas=ebe_vector)$minimum
  
}

CalculateIndCognitionDensity <- function(data_df,bAddHAZ,availCov,age,hazcurve,vars=c("IQ11","IQ15","ENG13","MAT13"),idistlength=1000,strLegend="",strCol="") {
  
  
  ### Remove covariates that are not possible to use for cognition
  covsToRemove<-c("FHTCM","NROOMS","DLVLOCN","HOMETYPE_4","HOMETYPE_3","HOMETYPE_2","RACEN_7","RACEN_6","RACEN_5","RACEN_3","RACEN_2","SITEID_5","SITEID_4","SITEID_3","SITEID_2")
  availCov<-availCov[!(availCov %in% covsToRemove)]
  
  varsName<-c("IQ11","IQ15","ENG11","MAT11")
  
  
  if (bAddHAZ) {
    TNADIR<-age[which(hazcurve==min(hazcurve))]
    HAZNADIR<-min(hazcurve)
    HAZ<-rep(0,12)
    AGE<-rep(0,12)
    for (i in seq(0,11,by=1)) {
      HAZ[i+1]<-hazcurve[which(abs(age-i)==min(abs(age-i)))]
      AGE[i+1]<-age[which(abs(age-i)==min(abs(age-i)))]
      data_df[[paste("HAZ",i,sep="")]]<-HAZ[i+1]
    }
    data_df[["TNADIR"]]<-TNADIR
    data_df[["HAZNADIR"]]<-HAZNADIR
    
    availCov<-c(availCov,paste("HAZ",0:11,sep=""),"TNADIR","HAZNADIR")
    #availCov<-c(availCov,"HAZNADIR")
  }
  
  missingIndex<-which(!(covnamescogn$covNames %in% availCov))
  
  OM_cogn<-OM_FULL
  
  cParamIndex<-c(53,54,55,56) #IQ11, IQ15, ENG13, MAT13
  
  OM_PAR     <- OM_cogn[cParamIndex,cParamIndex] #The parameter covariance matrix
  
  if (is.null(availCov) || length(availCov)==0)  {#If no covariates, let the variance be OM_PAR
    COEFF_VAR=OM_PAR
  } else {
  
  OM_COV     <- OM_cogn[-missingIndex,-missingIndex] #The covariates covariance matrix
  OM_PAR_COV <- OM_cogn[cParamIndex,-missingIndex] #The covariance between covariates and parameters matrix
  OM_PAR_COV <-as.matrix(OM_PAR_COV)
  
  inv       <- solve(OM_COV)
  COEFF     <- OM_PAR_COV%*%inv #The parameter-covariate coefficients 
  COEFF_VAR <- OM_PAR-OM_PAR_COV%*%inv%*%t(OM_PAR_COV) #The cognitive iiv variances given all covariates (except cognitive covariates)
  
  #Get the covariates that are available, i.e. all except the ones that considered parameters
  df_thm_nonmiss<-df_thmcogn[-missingIndex]
  covnamesorder<-covnamescogn$covNames[-missingIndex]
  myExpr <- c()
  for(p in 1:nrow(COEFF)) {
    myExpr[p] <- "" 
    for(c in 1:ncol(COEFF)) {
      myExpr[p] <- paste(myExpr[p],"COEFF[",p,",",c,"]","*","(data_df$",covnamesorder[c],"-",df_thm_nonmiss[c],")",sep="")
      
      if(c!=ncol(COEFF)) myExpr[p] <- paste(myExpr[p],"+")
    }
  }
  }

  cov_contribution<-rep(0,nrow(COEFF))
  if (bAddHAZ==TRUE) {
    for (p in 1:nrow(COEFF)) {
      cov_contribution[p]<-eval(parse(text=myExpr[p]))
    }
  }
  
  df<-data.frame()
  for (i in 1:length(varsName)) {
    sd<-sqrt(COEFF_VAR[i,i])
    me=COGN_MEAN[i]+cov_contribution[i]
    x<-seq(me-4*sd,me+4*sd,length.out = idistlength)
    df<-rbind(df,data.frame(value=x,density=dnorm(x = x,mean = me,sd = sd),TEST=varsName[i],TYPE=strLegend,COL=strCol))
  }
  ret<-list(df,COGN_MEAN,cov_contribution,COEFF_VAR)
  return(ret)
}


#Make the cognition figure
cognitionVarCurve <- function(age,hazcurve,haz_return,covoptions) { #Produce a new plot for explained/unexplained variability
  
  data_df<-haz_return[[4]]
  availCov<-haz_return[[5]]
  
  vars<-c("IQ11","IQ15","ENG13","MAT13")
  varsName<-c("IQ11","IQ15","ENG11","MAT11")
  width=0.3

  if (covoptions=="both") COEFF_VAR_CURR_COV<-CalculateIndCognitionDensity(data_df,bAddHAZ = TRUE,availCov,age,hazcurve,vars=vars,idistlength=1000,strLegend = "C",strCol="C")[[4]]
  if (covoptions=="onlyCov") COEFF_VAR_CURR_COV<-CalculateIndCognitionDensity(data_df,bAddHAZ = FALSE,availCov,age,hazcurve,vars=vars,idistlength=1000,strLegend = "C",strCol="C")[[4]]
  if (covoptions=="onlyHAZ") COEFF_VAR_CURR_COV<-CalculateIndCognitionDensity(data_df,bAddHAZ = TRUE,availCov="",age,hazcurve,vars=vars,idistlength=1000,strLegend = "C",strCol="C")[[4]]
  
  colnames<-c("Individual variability","Explained covariate variability","Unexplained covariate variability")
  df<-data.frame()
  for (i in 1:length(vars)) {
    df<-rbind(df,data.frame(NAME=varsName[i],COL=colnames[1],Y=COEFF_VAR_COGN_ALL_COV[i,i]/COEFF_VAR_COGN_NO_COV[i,i]*100,X=varsName[i]))
    df<-rbind(df,data.frame(NAME=varsName[i],COL=colnames[2],Y=(1-COEFF_VAR_CURR_COV[i,i]/COEFF_VAR_COGN_NO_COV[i,i])*100,X=varsName[i]))
    df<-rbind(df,data.frame(NAME=varsName[i],COL=colnames[3],X=varsName[i],Y=(COEFF_VAR_CURR_COV[i,i]-COEFF_VAR_COGN_ALL_COV[i,i])/ COEFF_VAR_COGN_NO_COV[i,i]*100))
  }
  
  cols<-c(cColorOrange,cColorLightBlue,cColorLightRed)
  names(cols)<-colnames
  
  p<-ggplot()
  
  p<-p+geom_bar(data=df,aes(y=Y,fill=COL,x=X),stat="identity",width = width)
  
  p<-p+ylab("Total variability (%)")+xlab("")
  p<-p+theme(legend.position="top",
             legend.title=element_blank(),
             legend.key.width = unit(1, "cm"),
             legend.key.size = unit(1, 'lines'),
             panel.ontop = FALSE)
  p<-p+scale_fill_manual(name="",values=cols)
  p<-p+coord_flip()
  
  return(p)
}
  

#Make the cognition figure
cognitionCurve <- function(age,hazcurve,df,strTitle="Cognition in the Phillipines",haz_return,dfobs,theta,om,sig,iOptions) { #Produce a new condition density given the individual covariate values
  
  data_df<-haz_return[[4]]
  availCov<-haz_return[[5]]

  vars<-c("IQ11","IQ15","ENG13","MAT13")
  varsName<-c("IQ11","IQ15","ENG11","MAT11")
  
  idistlength=1000
  df<-data.frame()
  for (i in 1:length(vars)) {
    sd<-sqrt(COEFF_VAR_COGN_NO_COV[i,i])
    x<-seq(COGN_MEAN[i]-4*sd,COGN_MEAN[i]+4*sd,length.out = idistlength)
    if (1 %in% iOptions) {
      df<-rbind(df,data.frame(value=x,density=dnorm(x = x,mean = COGN_MEAN[i],sd = sd),TEST=varsName[i],TYPE="A",COL="A")) # No covariates and no expected HAZ, OM_VAR
      
    }
  }

  if (2 %in% iOptions) {
    df<-rbind(df,CalculateIndCognitionDensity(data_df,bAddHAZ = TRUE,availCov=c(),age,typical,vars=vars,idistlength=idistlength,strLegend = "B",strCol="B")[[1]]) # No covariates but expected HAZ
  }
  
  if (3 %in% iOptions) { #Covariates and expected HAZ
    df<-rbind(df,CalculateIndCognitionDensity(data_df,bAddHAZ = TRUE,availCov,age,hazcurve,vars=vars,idistlength=idistlength,strLegend = "C",strCol="C")[[1]])
  }
  
  if (!is.null(dfobs) && nrow(dfobs)>0)  {
    #Calculate EBEs given the observations
    modelfunc<-function(age,thetas,etas) {return(calcHAZVector(age,basethetas = thetas[1:6],covthetas=thetas[7:12],etas=etas))}
    
    ebe_est<-ind_estimates(cData = as.numeric(dfobs$Haz),cTime = as.numeric(dfobs$Age),theta = theta,om = om,sig = sig,start_eta = rep(0,6),modelfunction=modelfunc)
    optimal_age<-get_next_sample_time(cData = as.numeric(dfobs$Haz),cTime = as.numeric(dfobs$Age),theta = theta,om = om,sig,ebe=ebe_est[[1]]) #Calculate next most informative sample
    haz<-calcHAZVector(age,theta,covthetas=theta[(noBaseThetas+1):length(theta)],etas = ebe_est[[1]])
    if (4 %in% iOptions) { #Covariates and individual HAZ
      df<-rbind(df,CalculateIndCognitionDensity(data_df,bAddHAZ = TRUE,availCov,age,haz,vars=vars,idistlength=idistlength,strLegend = "D",strCol="D")[[1]])
    }
  }
    
  df$COL<-factor(df$COL)
  cols<-c("black","grey","blue","red")
  cols<-c(cColorLightBlue,plotTitleColor,plotGridLinesColor, "grey")
  cols<-c(plotTitleColor,plotGridLinesColor,cColorLightBlue, "grey")
  
  names(cols)<-c("A","B","C","D")
  p<-ggplot()
  if (!is.null(iOptions)) {
    p<-p+geom_line(data=df,aes(x=value,y=density,group=TYPE,color=COL),size=lineSize) #Add all stored plots (+ the current hazcurve)
    p<-p+facet_wrap(~TEST)
  }
  p<-p+ylab("Density")+xlab("Cognition")
  p<-p+guides(linetype=FALSE)
  p<-p+scale_colour_manual(name="",values=cols)
  p<-p+ggtitle(strTitle)
  p<-p+theme(legend.position="top",
             legend.justification = c("center", "center"),
             plot.margin = unit(c(0, 0, 0, 0), "cm"),
             legend.title = element_text(margin = margin(b = 0)),
             panel.ontop = FALSE)
  return(p)
  
}

makeModelCurve<-function(age,hazcurve,df,strTitle="Expected HAZ versus age"){
  
  #Add new line
  df<-rbind(df,data.frame("AGE"=age,"HAZ"=hazcurve$data,"COL"=hazcurve$legend,"LT"=hazcurve$linetype,"COLORNAME"=hazcurve$color))
    
  cols<-as.character(df[!duplicated(df$COL),"COLORNAME"])
  strLegend<-levels(unique(df$COL))
  names(cols)<-strLegend
  
  ncols<-ceiling(length(cols)/3) #Number of columns for legend
  
  p<-ggplot()
  p<-p+geom_hline(aes(yintercept=-2),color=cColorOrange,size=lineSize,linetype=strStuntedLineType) #Add stunted red line
  p<-p+geom_line(data=df,aes(x=AGE,y=HAZ,color=COL,linetype=LT),size=lineSize) #Add all stored plots (+ the current hazcurve)
  p<-p+ylab("HAZ")+xlab("Age (years)")
  p<-p+coord_cartesian(ylim = c(lowlimitHAZ,highlimitHAZ))
  p<-p+scale_colour_manual(name="Covariates",values=cols,
                    guide = guide_legend(override.aes=aes(fill=NA),ncol=ncols))
  p<-p+guides(linetype=FALSE)
  p<-p+scale_x_continuous(breaks = 0:15)
  p<-p+ggtitle(strTitle)
  return(p)
}

makeModelCurvePop<-function(age,poplist,plotIndex,strTitle="Population HAZ versus age") {
  if (plotIndex==1) { #IIV HAZ vs Age plot
    df<-poplist[[1]][[1]]
    ymin<-poplist[[1]][[2]]
    ymax<-poplist[[1]][[3]]
    cols<-as.character(df[!duplicated(df$COL),"COLORNAME"])
    strLegend<-levels(unique(df$COL))
    names(cols)<-strLegend
    p<-ggplot()
    p<-p+geom_ribbon(aes(x=age,ymin=ymin,ymax=ymax),fill=cColorWhite,color=NA,alpha=0.8)
    
    p<-p+geom_hline(aes(yintercept=-2),color=cColorOrange,size=lineSize,linetype=strStuntedLineType) #Add stunted red line
    p<-p+geom_line(data=df,aes(x=AGE,y=HAZ,color=COL,linetype=LT),size=lineSize) #Add all stored plots (+ the current hazcurve)
     
     
     
    p<-p+ylab("HAZ")+xlab("Age (years)")
    p<-p+coord_cartesian(ylim = c(lowlimitHAZ,highlimitHAZ))
    p<-p+scale_colour_manual(name="",values=cols,
                              guide = guide_legend(override.aes=aes(fill=NA)))
    p<-p+guides(linetype=FALSE)
    p<-p+scale_x_continuous(breaks = 0:15) 
    p<-p+ggtitle(strTitle)
    return (p)
  }
  if (plotIndex==2) { #%stunted vs age
    
    stuntfrac<-poplist[[2]][[1]]
    tnadirindex<-poplist[[2]][[2]]
    TNADIR<-age[tnadirindex] #Get the rough pop time of nadir
    nadirtext<-paste(round(stuntfrac[tnadirindex]*100,1),"%",sep="")
    t2text<-paste(round(stuntfrac[length(age)]*100,1),"%",sep="")
    maxstunt<-max(stuntfrac)*100
     
    p1<-ggplot()
    p1<-p1+geom_line(aes(x=age,y=stuntfrac*100),color=plotTitleColor,size=lineSize)
    p1<-p1+geom_vline(aes(xintercept = TNADIR),linetype="dotted")
    p1<-p1+geom_text(aes(x = TNADIR,y=stuntfrac[tnadirindex]*100,label=nadirtext,fontface="bold"),nudge_x=0.3,nudge_y=maxstunt*0.05)
    p1<-p1+geom_vline(aes(xintercept = t2),linetype="dotted")
    p1<-p1+geom_text(aes(x = t2,y=stuntfrac[length(age)]*100,label=t2text,fontface="bold"),nudge_x=-.5,nudge_y=maxstunt*0.05)
    p1<-p1+ylab("Stunted (%)")+xlab("Age (years)")
    p1<-p1+scale_x_continuous(breaks = 0:15) 
    p1<-p1+ggtitle("Expected fraction of stunted")
    return (p1)
  }
  
  if (plotIndex==3) { #Distributions of nadir and tnadir
     dfdist<-poplist[[3]]
     p2<-ggplot(data=dfdist)
     p2<-p2+geom_line(aes(x=value,y=density),size=lineSize)
     p2<-p2+geom_vline(data=subset(dfdist,TYPE=="HAZ at nadir"),aes(xintercept=-2),color=cColorOrange,size=lineSize,linetype=strStuntedLineType)
     p2<-p2+xlab("")+ylab("Density")
     p2<-p2+theme(plot.title = element_text(colour = plotTitleColor,hjust=0.5, face="bold",margin = margin(b = 40)))
     p2<-p2+ggtitle("Expected density at nadir")
     p2<-p2+facet_wrap(~TYPE,scales = "free_x")
     return (p2)
  }
  
}

makeModelCurvePopCalc<-function(age,hazcurve,theta,om,strTitle="Population HAZ versus age"){
  
  #Add new line
  df<-data.frame("AGE"=age,"HAZ"=hazcurve$data,"COL"="Expected using known covariates","LT"=hazcurve$linetype,"COLORNAME"=hazcurve$color)

  eta_mean<-rep(0,6)
  eta_cov<-om
  var=rep(0,length(age))
  stuntfrac<-rep(0,length(age))
  
  
  if (bUseSampling) { ###First correlate the iiv samples to the new OM_matrix
    Meps<-iivsamples%*%chol(om) #Calculate correlated samples from om covariance matrix
    haz<-matrix(0,ncol=length(age),nrow=numIIVSamples)
    for (i in 1:numIIVSamples) {
        haz[i,]<-calcHAZVector(age,theta[1:6],theta[7:12],Meps[i,])
    }
    ymin=rep(0,length(age))
    ymax=rep(0,length(age))
    for (i in 1:length(age)) {
      tmp<-quantile(haz[,i],probs=c((1-popvariance/100)/2,1-(1-popvariance/100)/2),names=FALSE) #Calculate the popvariance Prediction interval
      ymin[i]<-tmp[1]
      ymax[i]<-tmp[2]
    }
    #Calculate percent stunted at each age based on simulations
    for (i in 1:length(age)) {
      stuntfrac[i]<-sum(haz[,i]< -2)/numIIVSamples
    }
    
    tnadirs<-rep(0,numIIVSamples)
    hnadirs<-rep(0,numIIVSamples)
    #Calculate the tnadir for each IIV sample
    for (i in 1:numIIVSamples) {
      tnadirs[i] <- age[which(haz[i,]==min(haz[i,]))] #Get the rough ind time of nadir
      hnadirs[i]<-min(haz[i,]) #Get the rough ind haz nadir
    }
    dst<-density(tnadirs,kernel = "rectangular",from=t1,to=t2) #Calculate the tnadir density
    dsh<-density(hnadirs,kernel = "rectangular") #Calculate the haznadir density
    dfdist<-data.frame(value=dst$x,density=dst$y,TYPE="Time of nadir (years)")
    dfdist<-rbind(dfdist,data.frame(value=dsh$x,density=dsh$y,TYPE="HAZ at nadir"))

  } else {
    for (i in 1:length(age)) {
      var[i]<-th_deltarule(eta_mean,eta_cov,wrapper_func,transform_derivFun=NULL,bIncludeCov=TRUE,theta,age[i])[2]
      stuntfrac[i]<-pnorm((-2 - hazcurve$data[i])/sqrt(var[i]))  #Calculate percent stunted at each age based on normal assumption
    }
    ymin=hazcurve$data+qnorm((1-popvariance/100)/2)*sqrt(var)
    ymax=hazcurve$data-qnorm((1-popvariance/100)/2)*sqrt(var)
    
    #Calculate nadir density
    tnadirexact<-optimize(calcHAZVector,interval=c(t1,t2),theta[1:6],covthetas=theta[7:12],etas=eta_mean)$minimum #Get exact model based tnadir
    tnadirvar<-th_deltarule(eta_mean,eta_cov,wrapper_func_tnadir,transform_derivFun=NULL,bIncludeCov=TRUE,theta)[2]
    haznadirexact<-calcHAZVector(age=tnadirexact,theta[1:6],covthetas=theta[7:12],etas=eta_mean) #Get exact model based haznadir
    haznadirvar<-th_deltarule(eta_mean,eta_cov,wrapper_func,transform_derivFun=NULL,bIncludeCov=TRUE,theta,tnadirexact)[2] #Get the variance of haz at nadir
    idistlength<-1000
    sdtnadir<-sqrt(tnadirvar)
    x<-seq(t1,t2,length.out = idistlength)
    # Cut dist by t1
    x<-x[x>=t1]
    dfdist<-data.frame(value=x,density=dnorm(x = x,mean = tnadirexact,sd = sdtnadir),TYPE="Time of nadir (years)")
    
    sdhaznadir<-sqrt(haznadirvar)
    x<-seq(haznadirexact-4*sdhaznadir,haznadirexact+4*sdhaznadir,length.out = idistlength)
    dfdist<-rbind(dfdist,data.frame(value=x,density=dnorm(x = x,mean = haznadirexact,sd = sdhaznadir),TYPE="HAZ at nadir"))
  }
  
  tnadirindex<-which(hazcurve$data==min(hazcurve$data)) #Get the index of tnadir
  return(list(list(df,ymin,ymax),list(stuntfrac,tnadirindex),dfdist))
}



get_next_sample_time<-function(cData,cTime,theta,om,sig,ebe) {
  
  mi_function<-function(newTime,cData,cTime,theta,om,sig,ebe) {
    haz<-calcHAZVector(newTime,theta,covthetas=theta[(noBaseThetas+1):length(theta)],etas = ebe) #Calculate expected HAZ at newTime
    #Add newTime and corresponding HAZ as new observation
    cData<-c(cData,haz)
    cTime<-c(cTime,newTime)
    
    modelfunc<-function(age,thetas,etas) {return(calcHAZVector(age,basethetas = thetas[1:6],covthetas=thetas[7:12],etas=etas))}
    #Calculate ebe and hessian using new observation
    ebe_est<-ind_estimates(cData =cData,cTime = cTime,theta = theta,om = om,sig = sig,start_eta = ebe,modelfunction=modelfunc) #Calculate EBE using one more sample
    return(1/det(ebe_est[[2]]))
  }
  offset<-1/12 #Minimum time to get back is last time + 1 month
  if (!is.null(cTime)) minTime<-max(cTime)+offset
  if (is.null(cTime)) minTime<-0
  #Get expected nadir given current observations
  tnadir<-optimize(calcHAZVector,interval=c(0,15),theta,covthetas=theta[(noBaseThetas+1):length(theta)],etas=ebe)$minimum
  maxTime<-tnadir
  if (tnadir<minTime) maxTime<-15
  return(optimize(mi_function,interval=c(minTime,maxTime),cData,cTime,theta,om,sig,ebe)$minimum)
}

makeModelCurveInd<-function(age,hazcurve,dfobs,theta,om,sig,strTitle="Individual predicted HAZ versus age"){
  
  #Add new line
  df<-data.frame("AGE"=age,"HAZ"=hazcurve$data,"COL"="Expected using known covariates","LT"=hazcurve$linetype,"COLORNAME"=hazcurve$color)
  
  cols<-as.character(df[!duplicated(df$COL),"COLORNAME"])
  strLegend<-levels(unique(df$COL))
  names(cols)<-strLegend
  
  p<-ggplot()
  optimal_age<-0 #The optimal (D-optimal) sample time given all observations
  if (!is.null(dfobs) && nrow(dfobs)>0)  {
    modelfunc<-function(age,thetas,etas) {return(calcHAZVector(age,basethetas = thetas[1:6],covthetas=thetas[7:12],etas=etas))}
    #Calculate EBEs given the observations
    ebe_est<-ind_estimates(cData = as.numeric(dfobs$Haz),cTime = as.numeric(dfobs$Age),theta = theta,om = om,sig = sig,start_eta = rep(0,6),modelfunction=modelfunc)
    optimal_age<-get_next_sample_time(cData = as.numeric(dfobs$Haz),cTime = as.numeric(dfobs$Age),theta = theta,om = om,sig,ebe=ebe_est[[1]]) #Calculate next most informative sample
    haz<-calcHAZVector(age,theta,covthetas=theta[(noBaseThetas+1):length(theta)],etas = ebe_est[[1]])
    hessian<-ebe_est[[2]] #The hessian of the Emperical Bayes Estimates (EBE)/Maximum a posteriori (MAP) estimates
    ebe_cov<-solve(hessian) #the covariance matrix of the ebe estimates
    var=rep(0,length(age))
    for (i in 1:length(age)) {
      var[i]<-th_deltarule(ebe_est[[1]],ebe_cov,wrapper_func,transform_derivFun=NULL,bIncludeCov=TRUE,theta,age[i])[2]
    }
    ymin=haz+qnorm((1-ebeuncertainty/100)/2)*sqrt(var)
    ymax=haz-qnorm((1-ebeuncertainty/100)/2)*sqrt(var)
    
    cols<-c(cols,plotTitleColor)
    strLegend<-c(strLegend,"Individual using known covariates")
    names(cols)<-strLegend
    p<-p+geom_ribbon(aes(x=age,ymin=ymin,ymax=ymax),alpha=0.8,color=NA,fill=cColorWhite)
    
  
  } else { #Mark most informative sample based on no samples, i.e. only expected curve
    optimal_age<-get_next_sample_time(cData = NULL,cTime = NULL,theta = theta,om = om,sig,ebe=rep(0,6))
  }
  p<-p+geom_line(data=df,aes(x=AGE,y=HAZ,color=COL,linetype=LT),size=lineSize) #Add all stored plots (+ the current hazcurve)
  p<-p+geom_hline(aes(yintercept=-2),color=cColorOrange,size=lineSize,linetype=strStuntedLineType) #Add stunted line
  
  if (!is.null(dfobs) && nrow(dfobs)>0)  {
    p<-p+geom_line(aes(x=age,y=haz,color="Individual using known covariates"),size=lineSize) #Plot EBE based curve
    p<-p+geom_point(data=dfobs,aes(x=Age,y=Haz),color=plotTitleColor,size=4) #Plot observation points
  }
  p<-p+geom_vline(aes(xintercept = optimal_age),linetype="dotted",color="black") #Add a vertical line for the optimal next sample time
  p<-p+ylab("HAZ")+xlab("Age (years)")
  p<-p+coord_cartesian(ylim = c(lowlimitHAZ,highlimitHAZ))
  p<-p+scale_colour_manual(name="",values=cols,
                           guide = guide_legend(override.aes=aes(fill=NA)))
  p<-p+guides(linetype=FALSE)
  p<-p+ggtitle(strTitle)
  p<-p+scale_x_continuous(breaks = 0:15) 
  return(p)
}


getIndividualHaz<-function(input) {
  availCov = c()
  data<-data.frame("MYROW"=1) #Just add a dummy row to be able to easily add values later on
  for (i in 1:length(covlist)) {
    tmp<-input[[paste0("n_input_", i)]] #Get the value from the input 
    if (!is.null(tmp) && !is.na(tmp) && tmp!=strMissing) {
      if (covlist[[i]]$Type==1) { #If a continuous value
        availCov<-c(availCov,names(covlist[i])) #Get all covariate names that has a non-missing value
        data[names(covlist[i])]<-tmp #Assign the non-missing value to a data frame
      } else { #Multi level covariate....
        if (length(covlist[[i]]$Values)==3) { #A dichotomous covariate  + missing {
          availCov<-c(availCov,names(covlist[i])) #Get all covariate names that has a non-missing value
          data[names(covlist[i])]<-covlist[[i]]$CodedValues[which(covlist[[i]]$Values==tmp)] #Assign the coded non-missing value to a data frame
        }
        if (length(covlist[[i]]$Values)>3) { #A polytomous covariate + missing, here all dichotomized covariates need to be taken into account
          selectedVal<-covlist[[i]]$CodedValues[which(covlist[[i]]$Values==tmp)] #Get the coded non-missing value (selected value)
          if (covlist[[i]]$Continuous==TRUE) {#Treat this variable as continuous
            availCov<-c(availCov,names(covlist[i])) #A covariate name that has a non-missing value treated as a continuous value
            data[names(covlist[i])]<-selectedVal #Assign the non-missing value to a data frame
          } else {
            for (j in 1:length(covlist[[i]]$CodedValues)) { #For all potential values
              varName<-paste0(names(covlist[i]),"_",covlist[[i]]$CodedValues[j])
              if (varName %in% covnames$covNames)  {#If this dichotomized variable is in the model
                availCov<-c(availCov,varName) #Get all covariate names that has a non-missing value
                #Set to 1 (true) if it is the same as the selectedVal, otherwise set to 0 (false)
                if (covlist[[i]]$CodedValues[j]==selectedVal) data[varName]<-1
                if (covlist[[i]]$CodedValues[j]!=selectedVal) data[varName]<-0
              }
            }
          }
        }
      }
    }
  }
  #Calculate the new covariate coefficients given the non-missing covariates
  ffem<-calcFFEM(dfext = dfext,noBaseThetas = noBaseThetas,noCovThetas = noCovThetas,noParCov = noBaseThetas,covNames = covnames$covNames,availCov = availCov, quiet = TRUE)
  covvalues<-rep(0,noBaseThetas)
  for (i in 1:noBaseThetas) {
    val<-eval(parse(text=ffem$Expr[[i]]))
    if (length(val)!=0) covvalues[i]<-val
  }
  #Calculate the individual haz versus age curve
  return(list(calcHAZVector(age = tim,basethetas = thbasevector,covthetas = covvalues),ffem,covvalues,data,availCov))
}

#The server function
server<-function(input,output){

 values <- reactiveValues(
  )
  
  values$df<-data.frame("AGE"=tim,"HAZ"=plotList[[1]]$data,"COL"=plotList[[1]]$legend,"LT"=plotList[[1]]$linetype,"COLORNAME"=plotList[[1]]$color)
  values$dfobs<-data.frame("Age"=numeric(0),"Haz"=numeric(0))
  values$element<-NULL
  
  
  addData <- observe({
    if (input$lockButton>0) {
      values$df <- isolate(rbind(values$df,data.frame("AGE"=tim,"HAZ"=getIndividualHaz(input)[[1]],"COL"=input$inputLegend,"LT"="filled","COLORNAME"=input$inputColor)))
    }
    })
  
  addObsData <- observeEvent(input$AddObsButton,{
    if (as.numeric(input$inputAge)>=0) values$dfobs <- isolate(rbind(values$dfobs,data.frame("Age"=as.numeric(input$inputAge),"Haz"=as.numeric(input$inputHaz))))
    })
  
  resetPlot <- observeEvent(input$resetButton,{
      values$df<-isolate(data.frame("AGE"=tim,"HAZ"=plotList[[1]]$data,"COL"=plotList[[1]]$legend,"LT"=plotList[[1]]$linetype,"COLORNAME"=plotList[[1]]$color))
  })
  
  resetData <- observeEvent(input$resetButtonData,{
    values$dfobs<-isolate(values$dfobs<-data.frame("Age"=numeric(0),"Haz"=numeric(0)))
  })
 
  
  #Create the select list of predefined scenarios
  output[["input_ui_select"]]<-renderUI({
    choices<-rep("",length(predeflist)) 
    for (i in 1:length(predeflist)){
      choices[i]<-predeflist[[i]]$DisplayName
    }
    choices<-c("<None>",choices)
    return (selectInput("predefSelect",label = h5("Predefined scenarios"),choices=choices))
  })
  
changePreDef <- observeEvent(input$predefSelect,{
                  values$element<-NULL
                  if (!is.null(input$predefSelect) && as.character(input$predefSelect)!="<None>") {
                    values$element<-predeflist[[as.character(input$predefSelect)]]
                  }
                  })


  #Create dynamic input based on covariates depending on covariate type and covariate levels
  lapply(1:numcolumns, function(ic) {
    output[[paste0("input_ui_",ic)]] <- renderUI({
      lapply(((ic-1)*numinputpercol+1):min(numinput,ic*numinputpercol), function(i) {
       strlabel<-covlist[[i]]$DisplayName
       value<-covlist[[i]]$Default #Set to default value if not any predefined values are to be used
        if (!is.null(values$element)) { #If a predefined value should be used instead
          if (names(covlist[i]) %in% values$element$CovariateVector) {#If the predef value is defined for this covariate
            value<-values$element$DisplayValues[which(names(covlist[i])==values$element$CovariateVector)]
          } 
        }
       
       if (covlist[[i]]$Unit!="") strlabel<-paste0(strlabel," (",covlist[[i]]$Unit,")")
       if (covlist[[i]]$Type==1) {
         strlabel<-paste0(strlabel," [",round(covlist[[i]]$Mean,3),"]")
         return(numericInput(paste0("n_input_", i), label = strlabel, value = value, min=covlist[[i]]$Min,max=covlist[[i]]$Max))
       }
       if (covlist[[i]]$Type==2) {
         return(radioButtons(paste0("n_input_", i), label = strlabel, choices = covlist[[i]]$Values, selected=value))
       }
       if (covlist[[i]]$Type==3) {
         return(selectInput(paste0("n_input_", i), label = strlabel, choices = covlist[[i]]$Values, selected=value))
       }
     })
  })
  })
  
  #Make the model figure
  output$modelcurve<-renderPlot({
    indvalue$data<-getIndividualHaz(input)[[1]]
    makeModelCurve(age=tim,hazcurve=indvalue,values$df) #Produce a new curve given the individual covariate values
    })
  #Make the individual model figure
  output$modelcurveind<-renderPlot({
    haz_return<-getIndividualHaz(input)
    indvalue$data<-haz_return[[1]]
    om<-haz_return[[2]]$Vars
    makeModelCurveInd(age=tim,hazcurve=indvalue,values$dfobs,c(thbasevector,haz_return[[3]]),om,sig) #Produce a new curve given the individual covariate values
  })
  
  
  values$poplist<-reactive({
    haz_return<-getIndividualHaz(input)
    indvalue$data<-haz_return[[1]]
    om<-haz_return[[2]]$Vars
    makeModelCurvePopCalc(age=tim,hazcurve=indvalue,c(thbasevector,haz_return[[3]]),om) #Produce a new curve given the population covariate values and IIV
  })
  
  #Make the population model figure
  output$modelcurvepop<-renderPlot({
    makeModelCurvePop(age=tim,values$poplist(),plotIndex=1)
  })
  
  #Make the percentage stunted
   output$stuntedcurve<-renderPlot({
     makeModelCurvePop(age=tim,values$poplist(),plotIndex=2)
   })
  
  #Make the distributions of tnadir and hnadir
   output$nadirdist<-renderPlot({
     makeModelCurvePop(age=tim,values$poplist(),plotIndex=3)
   })
  
  #Make the cognition figure
  output$cognitioncurve<-renderPlot({
    haz_return<-getIndividualHaz(input)
    cognitionCurve(age=tim,hazcurve=haz_return[[1]],values$df,"",haz_return,
                   values$dfobs,c(thbasevector,haz_return[[3]]), haz_return[[2]]$Vars,sig,
                   input[["cognitionOptions"]]) #Produce a new curve given the individual covariate values
  })
  
  output$explainedcognvar<-renderPlot({
    haz_return<-getIndividualHaz(input)
    cognitionVarCurve(age=tim,hazcurve=haz_return[[1]],haz_return,input[["cognvaroptions"]])
  })
  
  output$tableObs <- renderTable(values$dfobs,include.rownames=FALSE)
  
}

shinyApp(ui=ui, server=server)