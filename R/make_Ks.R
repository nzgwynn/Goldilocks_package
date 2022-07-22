make.Ks = function(M, vars, D, name, Plot, S, ToC){
  r.I = length(vars)
  No.cols = length(vars[[1]])
  I = matrix(NA, nrow = r.I, ncol = No.cols)
  for(i in 1:r.I) I[i,] = unlist(vars[[i]])

  dimnames(I) = list(rep(NULL, r.I), c("cols", "w", "L", "Mins", "Maxs"))

  ## No. of participant hospitals
  N = dim(D)[1]

  mymat = sdmat = matrix(NA, nrow = N, ncol = r.I)

  ## Combining columns
  for(i in 1:r.I) mymat[,i] = D[,I[i,"cols"]]

  ## Standardising and adding weights
  col.means = colMeans(mymat)
  col.sds = apply(mymat, 2, sd)
  w = as.numeric(I[,"w"])
  for(i in 1:r.I) sdmat[,i] =
    w[i]*(mymat[,i] - col.means[i])/col.sds[i]

  ## Making the distance matrix
  dist_mat_obj = dist(sdmat, diag = TRUE, upper = TRUE)
  dist_mat = as.matrix(dist_mat_obj)

  ## Telling the computer wich method to use to solve
  t_max = 60*5
  solver = S
  approximate = 0
  solver = list(name = solver, t_max = t_max, approximate = approximate,
                round_cplex = 0, trace_cplex = 0)

  ## Solving
  out = designmatch::nmatch(dist_mat = dist_mat, total_pairs = floor(N/2),
                            solver = solver, subset_weight = NULL)

  # These guys have the row numbers of the matched pairs
  id_1 = out$id_1
  id_2 = out$id_2
  Ms = data.frame(cbind(id_1, id_2))
  colnames(Ms) = c("Row number","Match")

  ## If there are any leftovers they get assigned where
  ## the user wants them assigned
  X = dim(D)[1] %% 2 == 0
  if(X == FALSE){
    LO = sum(1:dim(D)[1]) - sum(id_1, id_2)
  }else{
    LO = NA
  }

  ## Will be used in the loop below - a place to put Ks,
  ## filled below, then graphed after using parallel coordinate plot
  Ks = matrix(NA, nrow = M, ncol = r.I)

  ## Used to run the loop below
  M.seq = seq_along(1:M)

  for(i in M.seq){
    ## Randomising once - 0 ctl, 1 trt, (subtract one because when
    ## making Trt below the middle step has to identify the length of
    ## Trt or it gets fussy and won't add on the last value)
    R = replicate(length(id_1), rbinom(1, size = 1, prob = 0.5))
    S = 1 - R
    Trt = c(R, S)

    ## Making the data
    if (X == TRUE) {

      ## If nothing is leftover we don't need ToC
      Dt = data.frame(mymat[c(id_1, id_2),], Trt)
    } else {

      ## If anything is, then we do
      ## ToC is 2 for trt and 1 for control, added at end
      Dt = data.frame(mymat[c(id_1, id_2, LO),], c(Trt, (ToC - 1)))
    }

    ## Picking out trt and ctl covariates
    Trt.CV = Dt[which(Trt == 1), 1:r.I]
    Ctl.CV = Dt[-which(Trt == 1), 1:r.I]

    Ks[i,] = abs((apply(Trt.CV, 2, sum) -
                    apply(Ctl.CV, 2, sum))/N)
  }## ending for i in M.seq

  ## So the labels in parcoord come out nicely
  ## NEW colnames here
  colnames(Ks) = I[,'L']

  P = list()
  P[[1]] = name ## name of file, not used
  P[[2]] = I ## matching things
  P[[3]] = as.data.frame(Ks) ## means after randomization
  P[[4]] = Ms ## The row numbers of the matches are in here
  P[[5]] = LO # leftover
  P[[6]] = X # this is TRUE if there are any leftover
  P[[7]] = Dt

  P
} ## closing the function make.Ks
