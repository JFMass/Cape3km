from __future__ import division
import matplotlib
matplotlib.use('Agg')
import pygrib
import math, sys
from subprocess import Popen, PIPE
import numpy as np
import numpy.ma as ma
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import threading
sys.path.append(os.path.expanduser('~/home/meteo'))
from cape import cape3

class profObjIdx(object):
	
	def __init__(self,filepath,model):
		
		
		#get from file
		with Popen(['/home/jfmass/uems/util/bin/wgrib2', filepath], stdout=PIPE, stderr=PIPE) as r:
			output, errore = r.communicate()
		prsrec = output.decode('utf-8').splitlines()
		grbit = pygrib.open(filepath)
		#get surface stuff
		self.grdHgt = grbit[int([s for s in prsrec if 'HGT:surface' in s][0].split(':')[0])].values
		self.lat, self.lon = grbit[int([s for s in prsrec if 'HGT:surface' in s][0].split(':')[0])].latlons()
		self.twoMtrDpt = grbit[int([s for s in prsrec if 'DPT:2 m above ground' in s][0].split(':')[0])].values
		self.twoMtrTmp = grbit[int([s for s in prsrec if 'TMP:2 m above ground' in s][0].split(':')[0])].values
		self.grdPrs = grbit[int([s for s in prsrec if 'PRES:surface' in s][0].split(':')[0])].values
		self.maxCape = grbit[int([s for s in prsrec if 'CAPE:255-0 mb above ground' in s][0].split(':')[0])].values      #useful to shorten calculations in cape3
		self.sbcape3 = np.zeros(self.maxCape.shape)     #create base array for 3km cape, fills with 0s as base
		self.mlcape3 = np.zeros(self.maxCape.shape)     #create base array for 3km cape, fills with 0s as base
		self.lsi = np.full(self.maxCape.shape,99)     #create base array for lid strenght index, fills with 99s since 0 is a good value
		self.xmax,self.ymax = self.sbcape3.shape   
		self.tmpH = ma.zeros((50,self.xmax,self.ymax))
		self.spfhH = ma.zeros(self.tmpH.shape)
		self.hgtH = ma.zeros(self.tmpH.shape)
		self.prsH = ma.zeros(self.tmpH.shape)
		self.vert = {}
		self.grbit = grbit
		self.prsrec = prsrec
		self.retrieve = ['TMP','SPFH','HGT','PRES']
		n=0
		while n < 50:
			self.tmpH[n:]=grbit[int([s for s in prsrec if 'TMP:'+str(n+1)+' hybrid level' in s][0].split(':')[0])].values
			self.spfhH[n:]=grbit[int([s for s in prsrec if 'SPFH:'+str(n+1)+' hybrid level' in s][0].split(':')[0])].values
			self.hgtH[n:]=grbit[int([s for s in prsrec if 'HGT:'+str(n+1)+' hybrid level' in s][0].split(':')[0])].values
			self.prsH[n:]=grbit[int([s for s in prsrec if 'PRES:'+str(n+1)+' hybrid level' in s][0].split(':')[0])].values
			n = n+1
		


	
	

def makeMap(var, lats, lons, title,levels):
	m = Basemap(epsg=3857,llcrnrlon=-125.4,llcrnrlat=28.3,urcrnrlon=-69.9,urcrnrlat=49,resolution='i')
	#m = Basemap(epsg=3857,llcrnrlon=-150,llcrnrlat=0,urcrnrlon=-69.9,urcrnrlat=49,resolution='i')
	m.drawcoastlines()
	m.drawstates()
	m.drawcountries()
	x, y =m(lons, lats)

	cs = m.contourf(x,y,var,levels)
	cbar = m.colorbar(cs,location='bottom',pad="5%")
	cbar.set_label('J/kg*m3')
	plt.savefig('/home/jfmass/'+title+'.png')
	plt.clf()	
			
			
path = sys.argv[1]

print('Creating profile base object')
prof = profObjIdx(path,'hrrr')

print('Done creating profile object')
mlcape3 = prof.mlcape3
sbcape3 = prof.sbcape3
lsi = prof.lsi

cape3, lsi = cape3.severe(prof.xmax,prof.ymax,50,prof.maxCape,prof.tmpH,prof.spfhH,prof.hgtH,prof.prsH,prof.grdHgt)
	

print('now print a beautiful map!')

makeMap(mltmp,prof.lat,prof.lon,'MLTMP',[250,255,260,265,270,275,280,285,290,295,300,305,310,315])
#makeMap(mlcape3,prof.lat,prof.lon,'SBCAPE3',[50,75,100,150,200])
#makeMap(lsi,prof.lat,prof.lon,'SBCAPE3',[0,1,2,3,5,7])




