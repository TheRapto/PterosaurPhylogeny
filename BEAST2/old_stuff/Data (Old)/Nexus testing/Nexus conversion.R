#required packages
require('ape')
require('dplyr')

Labels = read.csv('Charstatelabels2.csv')

Removed = read.table('Removed-characters.txt')

NewLabels <- Labels[-c(Removed$x), ]

Newcount=c(1:609)

NewLabels$T=Newcount

for (i in NewLabels) {
  NewLabels$X[i]=paste("'",NewLabels$X[i],"'")
  NewLabels$X.2[i]=paste("'",NewLabels$X.2[i],"'")
  NewLabels$X.4[i]=paste("'",NewLabels$X.4[i],"'")
  NewLabels$X.6[i]=paste("'",NewLabels$X.6[i],"'")
  NewLabels$X.8[i]=paste("'",NewLabels$X.8[i],"'")
  NewLabels$X.10[i]=paste("'",NewLabels$X.10[i],"'")
  NewLabels$X.12[i]=paste("'",NewLabels$X.12[i],"'")
  NewLabels$X.14[i]=paste("'",NewLabels$X.14[i],"'")
  NewLabels$X.16[i]=paste("'",NewLabels$X.16[i],"'")
}


write.csv(NewLabels,file = "NewCharstatelabels.csv")

