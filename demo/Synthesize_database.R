#Create a simple database with 2 tables: TableA and TableB.

data(tableA);
data(tableB);
uniqueid<-unique(tableA[,1:2])
tableB<-cbind(uniqueid,tableB[1:nrow(uniqueid),])

#Transpose the two tables: one row per unique value of id1a, id1b.

TKtableA<-BigSyn::Generaltransposefunction(tableA,c("id1a","id1b"),c("id2a","id2b"))
TKtableB<-BigSyn::Generaltransposefunction(tableB,c("id1a","id1b"),character(0))


#Merge everything

Ttable<-merge(TtableA,TtableB)

#Synthesize

STtable<-SDPSYN2(Ttable,asis = c("id1a","id1b"),nrep = 1)

#Separate the Synthetic merged transposed table by table of origin

STtableA<-STtable[[1]][c("id1a","id1b",grep("tableA",names(STtable[[1]]),value = TRUE))]
STtableB<-STtable[[1]][c("id1a","id1b",grep("tableB",names(STtable[[1]]),value = TRUE))]

#Back transpose

TSTtableA<-BigSyn::GeneralReversetransposefunction(TtableA = STtableA,
                                                  key = TKtableA$key)

TSTtableB<-BigSyn::GeneralReversetransposefunction(TtableA = STtableB,
                                                   key = TKtableB$key)

#check the two tables
runCompare(TSTtableA,TSTtableB)
