# Cascade plots for FS & BNY
# Date: 25/5/2016
# Author: Lars Dalby

library(data.table)
library(readxl)
library(ggplot2)
library(RColorBrewer)

ScipenDefault = getOption('scipen')  # In case it should be turned back again.
options(scipen = 99)  # To avoid scientific notation in the resulting file

muni = as.data.table(read_excel('C:/Users/lada/Desktop/NatKvalIndex2.xlsx'))
munisID = unique(muni[,KommuneID])
# Fix multibioscore error in data:
muni = unique(muni[, Areal:=sum(Areal), by = c('KommuneID', 'Type', 'Bioscore', 'Navn')])
# Calc the proportions
muni[, Areal:=Areal/10000]
muni[, ArealTotal:=sum(Areal), by = 'KommuneID']
muni[, TypeAreal:=sum(Areal), by = c('KommuneID', 'Type')]
muni[, TypeAndel:=(TypeAreal/ArealTotal)*100, by = c('KommuneID', 'Type')]
muni[, ScoreAndel:=(Areal/TypeAreal)*100, by = c('KommuneID', 'Type')]
muni[, ymax:=cumsum(ScoreAndel), by = c('KommuneID', 'Type')]
muni[, ymin:=ymax - ScoreAndel]
# Area of type:
typen = unique(muni[,.(KommuneID, Type, TypeAndel)])
typen[,xmax:=cumsum(TypeAndel), by = 'KommuneID']
typen[,xmin:=xmax - TypeAndel, by = 'KommuneID']
# Merge them back together:
final = merge(muni, typen, by = c('KommuneID', 'Type'))
final[,AvgScore:=log(weighted.mean(Bioscore, Areal)+1), by = c('KommuneID', 'Type')]
final[TypeCode == 6, AvgScore:=0.77]
final[Type == 'ByerHuseVeje', AvgScore:=0.26/4]
andet = copy(final)
andet = unique(andet[, .(KommuneID, Type, AvgScore, TypeAndel.x)])
andet = unique(andet[Type != 'Andet', AvgScore:=weighted.mean(AvgScore, TypeAndel.x), by = c('KommuneID')][Type != 'Andet',.(KommuneID, AvgScore)])
# Put the new Andet scores back into final:
final[Type == 'Andet', AvgScore:=andet[,AvgScore]]
final[AvgScore > log(13), AvgScore:=log(13), by = c('KommuneID', 'Type')]

# Beregn naturkapital index:
natind = unique(final[, .(KommuneID, Type, xmax, xmin, AvgScore)])
natind[,x:=xmax-xmin, by = KommuneID]
natind[,TypeAreal:=x*AvgScore, by = KommuneID]
natind[,NatKapInd:=round((sum(TypeAreal)/(log(13)*100))*100), by = KommuneID]

# Skriv tabel med KommuneID og NatKapInd:
write.table(unique(natind[,.(KommuneID, NatKapInd)]), file = 'C:/Users/lada/Desktop/NatKapInd19092016.txt', sep = '\t', row.names = FALSE, quote = FALSE)
write.table(natind, file = 'C:/Users/lada/Desktop/NatKapInd19092016.txt', sep = '\t', row.names = FALSE, quote = FALSE)

# Set up the color scheme:
dark2 = brewer.pal(8, 'Dark2')
set2 = brewer.pal(8, 'Set2')
set3 = brewer.pal(9, 'Set3')
set1 = brewer.pal(9, 'Set1')
past1 = brewer.pal(9, 'Pastel1')
past2 = brewer.pal(8, 'Pastel2')
cols = c("Skov" = dark2[5],
 		"Mark" = set3[6],
  		"HedeOverdrev" = past2[4],
   		"EngMose" = past1[2],
		"Sø" = set1[2],
	 	"ByerGrønt" = set2[5],
	  	"ByerHuseVeje" = set1[9],
	   	"Andet" = brewer.pal(9, 'Greys')[2])  #dark2[8]
# Plot:
pdf(file = 'C:/Users/lada/Desktop/NatKvalIndex18_LD.pdf')
for (i in seq_along(munisID)) {
	komnavn = unique(final[KommuneID == munisID[i],Navn])
	NatKapInd = unique(natind[KommuneID == munisID[i], NatKapInd])
	komnavn = paste(komnavn, round(NatKapInd), sep = ' - ')
	p = ggplot(final[KommuneID == munisID[i],], aes(ymin = 0, ymax = AvgScore, xmin = xmin, xmax = xmax,
		fill = factor(Type))) + ylab('Naturværdi') + xlab('Arealandel (%)') + ggtitle(komnavn)
	p = p + geom_rect() + theme_bw() + 
	scale_y_continuous(breaks = seq(0, log(13), length = 6)[1:5], limits = c(0,log(13)), labels = c('Ingen', 'Lille', 'Lokal', 'Regional', 'National')) + 
	theme(panel.grid.minor = element_blank()) + scale_fill_manual(values = cols, guide = guide_legend(title = "Type")) + 
	scale_x_continuous(breaks = seq(0, 100, length.out = 11))
	print(p)
}
dev.off()

colour = I("grey")