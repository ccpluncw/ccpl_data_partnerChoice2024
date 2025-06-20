###############################
#### MODEL SPECIFICATIONS #####
###############################



#in this model: positive coefficients for the "s" parameter indicate a bias toward the "keep" boundary and negative coefficients indicate a bias toward the "donate" boundary
grpVars <- c("leftItem","traitQuantity")


#freeNSD
#Add NSD as a fixed parameter (NSD = 1)
#Here I group by the three variables but add not effects for them.  This will split the data properly
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- grpVars
  parameter <- "nSD"
  ParameterName <- "nSD"
  parameterBounds <- c(7, 1, 0.25)

  freeNSD  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #freeDB
  #Add DB as a free parameter
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "db"
  ParameterName <- "db"
  parameterBounds <- c(0.5, 0, 0.001)

  freeDB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #freeB
  #Add B as a free parameter
  #add a column for the fixed parameter.  This is needed because we will be adding another b effect later
    x1 <- "default"
    #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
    v1 <- 1

  columnName <- "bConstant"
  df.code <- data.frame(logic = c(x1), value = c(v1))
  GroupByVariables <- grpVars
  parameter <- "b"
  ParameterName <- "bConstant"
  parameterBounds <- c(200, 5, 1)

  freeB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


    #traitQuantityVC
    #add value change model
      x1 <- "traitQuantity == 'HVO_2-LVO_1'"
      v1 <- 1
      x2 <- "traitQuantity == 'HVO_1-LVO_2'"
      v2 <- -1
      x3 <- "default"
      v3 <- 0

    columnName <- "traitQuantityVCColumn"
    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
    GroupByVariables <- grpVars
    ParameterName <- "vTraitQuantity"
    parameter <- "vc"
    parameterBounds <- c(5, -5, 0.01)

    traitQuantityVC  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

    #traitQuantityB
    #add Boundary model
      x1 <- "traitQuantity == 'HVO_2-LVO_2'"
      v1 <- 1
      x2 <- "traitQuantity == 'HVO_1-LVO_1'"
      v2 <- -1
      x3 <- "default"
      v3 <- 0

    columnName <- "traitQuantityBColumn"
    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
    GroupByVariables <- grpVars
    ParameterName <- "bTraitQuantity"
    parameter <- "b"
    parameterBounds <- c(200, 0, 1)

    traitQuantityB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

		#sideBiasSE
    #now predict the split of "leftItem" by a shift in the startpoint (s).  Do that by adding a dummy coded dataframe (df.code)
    #Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
      x1 <- "leftItem == 'leftLVO'"
      #when the leftItem is LVI then bias towards the upperBound (save HVI: bias = no action)
      v1 <- 1
      x2 <- "leftItem == 'leftHVO'"
      #when the leftItem is HVI then bias towards the lowerBound (save LVI: bias = no action)
      v2 <- -1
      x3 <- "default"
      v3 <- 0

      #this is the columnName of the dummy/effect variable
      columnName <- "leftItemSEColumn"
      #this dataframe contains the coding inforamtion of the dummy/effect variable
      df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
      #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
      GroupByVariables <- grpVars
      #this is the name given to the parameter that will measure the effect of this dummy/effect variable
      ParameterName <- "sLeftItembias"
      #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
      parameter <- "s"
      #These are the bounds of the parameter values: c(high, low, interval)
      parameterBounds <- c(0.8, -0.8, 0.001)

    #add them to an existing model: here we add them to the simple model to create the overall Start Effect Model
    leftItemSE  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

#########################
### build models
#########################

simpleModelList <- NULL

#simple model
simpleModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(freeNSD, freeDB, freeB))

#single biases
liSEModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(leftItemSE))

tqBModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(traitQuantityB))

tqVCModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(traitQuantityVC))

#two way biases
tqBliSEModelList <- rrwAddParameterEffectListToRRWModel(liSEModelList, c(traitQuantityB))

tqVCliSEModelList <- rrwAddParameterEffectListToRRWModel(liSEModelList, c(traitQuantityVC))

tqVCtqBModelList <- rrwAddParameterEffectListToRRWModel(tqBModelList, c(traitQuantityVC))

#three way biases
tqVCtqBliSEModelList <- rrwAddParameterEffectListToRRWModel(tqVCtqBModelList, c(leftItemSE))



allModels <- list(
  tqBModelList.kfold = tqBModelList
)

allFixedModels <- NULL
