#' SetupCPPAndStructure
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom sykdomspuls GenNorwayMunicipMerging
#' @importFrom withr with_dir
#' @importFrom processx run
#' @export SetupCPPAndStructure
SetupCPPAndStructure <- function(){
  . <- NULL
  locationNew <- NULL
  location <- NULL
  pop <- NULL
  n <- NULL
  d <- NULL
  from <- NULL
  to <- NULL
  kommuneNameOld <- NULL
  fromNew <- NULL
  toNew <- NULL


  unlink(CONFIG_DIR$DIR_TMP, recursive = TRUE, force = TRUE)
  dir.create(CONFIG_DIR$DIR_TMP)

  file.copy(file.path(CONFIG_DIR$DIR_SRC,list.files(CONFIG_DIR$DIR_SRC)), CONFIG_DIR$DIR_TMP)
  file.copy(file.path(CONFIG_DIR$DIR_DATA,list.files(CONFIG_DIR$DIR_DATA,pattern="xlsx")), CONFIG_DIR$DIR_TMP)

  pop_wo_com <- data.table(readxl::read_excel(file.path(CONFIG_DIR$DIR_TMP,sprintf("%s.xlsx","pop_wo_com"))))
  di_edge_list <- data.table(readxl::read_excel(file.path(CONFIG_DIR$DIR_TMP,sprintf("%s.xlsx","di_edge_list"))))

  loc <- pop_wo_com[,c("kommuneNameOld","location")]
  setnames(loc,"kommuneNameOld","from")
  loc[,from:=factor(from,levels=from)]

  nrow(di_edge_list)
  di_edge_list <- merge(di_edge_list,loc,by="from")
  nrow(di_edge_list)
  di_edge_list[,from:=NULL]
  setnames(di_edge_list,"location","from")

  setnames(loc,"from","to")
  nrow(di_edge_list)
  di_edge_list <- merge(di_edge_list,loc,by="to")
  nrow(di_edge_list)
  di_edge_list[,to:=NULL]
  setnames(di_edge_list,"location","to")
  setcolorder(di_edge_list,c("from","to","n"))

  pop_wo_com[,kommuneNameOld:=NULL]
  setcolorder(pop_wo_com,c("location","pop"))



  aMaster <- sykdomspuls::GenNorwayMunicipMerging()
  for(i in unique(aMaster$year)){
    a <- unique(aMaster[year==i,c("municip","municipEnd")])
    setnames(a,c("from","fromNew"))
    nrow(di_edge_list)
    di_edge_list <- merge(di_edge_list,a,by="from",all.x=T)
    nrow(di_edge_list)
    di_edge_list[!is.na(fromNew),from:=fromNew]
    di_edge_list[,fromNew:=NULL]

    setnames(a,c("to","toNew"))
    nrow(di_edge_list)
    di_edge_list <- merge(di_edge_list,a,by="to",all.x=T)
    nrow(di_edge_list)
    di_edge_list[!is.na(toNew),to:=toNew]
    di_edge_list[,toNew:=NULL]

    setnames(a,c("location","locationNew"))
    nrow(pop_wo_com)
    pop_wo_com <- merge(pop_wo_com,a,by="location",all.x=T)
    nrow(pop_wo_com)
    pop_wo_com[!is.na(locationNew),location:=locationNew]
    pop_wo_com[,locationNew:=NULL]
  }

  pop_wo_com <- pop_wo_com[,.(pop=sum(pop)),by=.(location)]
  di_edge_list <- di_edge_list[from!=to,.(n=sum(n)),by=.(from,to)]

  length(unique(pop_wo_com$location))
  length(unique(d$location))

  setorder(di_edge_list,from,to)
  setorder(pop_wo_com,location)


  for(i in c("di_edge_list","pop_wo_com")){
    fwrite(get(i),
           file=file.path(CONFIG_DIR$DIR_TMP,sprintf("%s.txt",i)),
           sep=" ",
           col.names=F)
  }

  res <- withr::with_dir(CONFIG_DIR$DIR_TMP,{
    processx::run(
      command="g++",
      args=c("-std=c++11","-oinfl_kommuner.exe","infl_kommuner.cpp"),echo=T)
  })
}

#' SetupStartInfected
#' @param d a
#' @param s a
#' @param startWeek a
#' @param doctorVisitingProb a
#' @param initialStartingMultiplier a
#' @import data.table
#' @export SetupStartInfected
SetupStartInfected <- function(d,s,startWeek,doctorVisitingProb,initialStartingMultiplier=2){
  season <- NULL
  age <- NULL

  fwrite(data.frame(floor(d[season==s & week==startWeek & age=="Totalt"]$n*doctorVisitingProb*initialStartingMultiplier)),
         file=file.path(CONFIG_DIR$DIR_TMP,"start_infected.txt"),
         sep=" ",
         col.names=F)

  return(d[season==s & week==startWeek & age=="Totalt"]$x[1])
}

#' RunSim
#' @param param a
#' @param d a
#' @param s a
#' @import data.table
#' @importFrom withr with_dir
#' @importFrom processx run
#' @export RunSim
RunSim <- function(param=c(0.6),d,s){
  . <- NULL
  kn <- NULL
  day <- NULL
  x <- NULL
  startX <- NULL
  shiftX <- NULL
  S <- NULL
  E <- NULL
  SI <- NULL
  AI <- NULL
  R <- NULL
  location <- NULL
  season <- NULL
  age <- NULL

  res <- withr::with_dir(CONFIG_DIR$DIR_TMP,{
    processx::run(
      command=file.path(CONFIG_DIR$DIR_TMP,"infl_kommuner.exe"),
      args=c(as.character(param),as.character(CONFIG_PAR$gamma),as.character(CONFIG_PAR$a)))
  })

  list.files(CONFIG_DIR$DIR_TMP)

  loc <- fread(file.path(CONFIG_DIR$DIR_TMP,"pop_wo_com.txt"))
  setnames(loc,c("location","pop"))
  loc[,kn:=1:.N-1]

  m <- fread(file.path(CONFIG_DIR$DIR_TMP,"cpp_res_series.txt"))
  setnames(m,c("kn","S","E","SI","AI","R"))
  m <- m[seq(1,nrow(d),2)]
  m[,day:=1:.N,by=kn]

  m <- merge(m,loc,by="kn")
  m[,x:=floor(day/7)+startX+shiftX]

  m <- m[,.(
    S=mean(S),
    E=mean(E),
    SI=mean(SI),
    AI=mean(AI),
    R=mean(R)),
    by=.(
      location,x
    )]

  res <- merge(d[season==s & age=="Totalt",],m,by=c("location","x"))

  return(res)
}

#' OptimFunction
#' @param param a
#' @param d a
#' @param s a
#' @param doctorVisitingProb a
#' @import data.table
#' @export OptimFunction
OptimFunction <- function(param=c(0.6),d,s,doctorVisitingProb){
  res <- RunSim(param=param,d=d,s=s)

  . <- NULL
  n <- NULL
  S <- NULL
  E <- NULL
  SI <- NULL
  AI <- NULL
  R <- NULL
  x <- NULL
  error <- NULL

  natRes <- res[,.(
    n=sum(n),
    S=sum(S),
    E=sum(E),
    SI=sum(SI),
    AI=sum(AI),
    R=sum(R)
  ),by=.(x)]

  natRes[,error:=n-SI*doctorVisitingProb]

  totalError <- mean(natRes$error^2)

  return(totalError)
}
