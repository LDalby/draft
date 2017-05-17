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
# muni = as.data.table(read_excel('O:/ST_Lada/Projekter/NKI/FlemmingTest/NatKvalIndex4.xlsx'))

# Fix multibioscore error in data:
muni = unique(muni[, Areal:=sum(Areal), by = c('KommuneID', 'Type', 'Bioscore', 'Navn')])

# foo = data.table(KommuneID = -1,
#                  Navn = 'bar',
#                  TypeCode = c(1:3, 8),
#                  Type = c('Skov', 'Mark', 'HedeOverdrev', 'Andet'),
#                  Bioscore = c(1, 6, 8, 0), Areal = c(500, 100, 50, 0))
# muni = rbind(muni, foo)
munisID = unique(muni[,KommuneID])
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
final = merge(muni, typen, by = c('KommuneID', 'Type'))  # okay hertil
final[,AvgScore:=log(weighted.mean(Bioscore, Areal)+1), by = c('KommuneID', 'Type')]
final[, BioScoreRaw:=weighted.mean(Bioscore, Areal), by = c('KommuneID', 'Type')]
final[TypeCode == 6, AvgScore:=0.77]
final[Type == 'ByerHuseVeje', AvgScore:=0.26/4]
andet = copy(final)
andet = unique(andet[, .(KommuneID, Type, AvgScore, TypeAndel.x)])
andet = unique(andet[Type != 'Andet', AvgScore:=weighted.mean(AvgScore, TypeAndel.x), by = c('KommuneID')][Type != 'Andet',.(KommuneID, AvgScore)])
# Put the new Andet scores back into final:
final[Type == 'Andet', AvgScore:=andet[,AvgScore]]
# andet = unique(andet[Type != 'h) andet', AvgScore:=weighted.mean(AvgScore, TypeAndel.x), by = c('KommuneID')][Type != 'h) andet',.(KommuneID, AvgScore)])
# Put the new Andet scores back into final:
# final[Type == 'h) andet', AvgScore:=andet[,AvgScore]]
final[AvgScore > log(13), AvgScore:=log(13), by = c('KommuneID', 'Type')]

# final_ld = as.tbl(copy(final))

# Beregn naturkapital index:
natind = unique(final[, .(KommuneID, Type, xmax, xmin, AvgScore)])
natind[,x:=xmax-xmin, by = KommuneID]
natind[,TypeAreal:=x*AvgScore, by = KommuneID]
natind[,NatKapInd:=round((sum(TypeAreal)/(log(13)*100))*100), by = KommuneID]

# Test against published values:
dn = as.data.table(read_excel('C:/Users/lada/Documents/Metodebeskrivelse-og-data/Bilag II - NKI vaerdier for hver enkelt kommune.xlsx'))
combined = merge(unique(natind[, .(KommuneID, NatKapInd)]), dn, by.x = "KommuneID", by.y = "KommID")

# OpsummÃ©r Bioscore, areal osv.
tmp = final[,.(KommuneID, Navn, Type, AvgScore, BioScoreRaw, xmin, xmax)][, Areal:=xmax-xmin]
tmp = final[,.(KommuneID, Navn, Type, AvgScore, BioScoreRaw, xmin, xmax)][, Areal:=xmax-xmin, by = c('KommuneID', 'Type')]
tmp = unique(tmp[,.(KommuneID, Navn, Type, AvgScore, BioScoreRaw, Areal)])
# Skriv tabel med KommuneID og NatKapInd:
write.table(unique(natind[,.(KommuneID, NatKapInd)]), file = 'C:/Users/lada/Desktop/NatKapInd19092016.txt', sep = '\t', row.names = FALSE, quote = FALSE)
write.table(natind, file = 'C:/Users/lada/Desktop/NatKapInd19092016.txt', sep = '\t', row.names = FALSE, quote = FALSE)
write.table(tmp, file = 'C:/Users/lada/Desktop/OpsummeretNatKapInd18102016.txt', sep = '\t', row.names = FALSE, quote = FALSE)

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

final[, Type:=factor(Type, levels = c("Skov", "Mark", "HedeOverdrev", "EngMose", "Sø", "ByerGrønt", "ByerHuseVeje", "Andet"))]

# Plot:
path = #sÃ¦t sti...
plotnavn = paste0(komnavn, Sys.Date(), '.pdf')
filnavn = file.path(path, plotnavn)
pdf(file = 'C:/Users/lada/Desktop/NatKvalIndex17052017_LD.pdf')
for (i in seq_along(munisID)) {
	#pdf(filnavn)
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
	#dev.off()
}
dev.off()

colour = I("grey")