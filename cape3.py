from __future__ import division
import matplotlib as mpl
mpl.use('Agg')
import pygrib
import math, sys, os
from subprocess import Popen, PIPE
import numpy as np
import numpy.ma as ma
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
sys.path.append(os.path.expanduser('~/home/meteo'))
from cape import cape3
from matplotlib.colors import LinearSegmentedColormap
from scipy.ndimage.filters import gaussian_filter

class profObjIdx(object):
	
	def __init__(self,filepath,model):
		
		
		#get from file
		with Popen(['/home/meteo/uems/util/bin/wgrib2', filepath], stdout=PIPE, stderr=PIPE) as r:
			output, errore = r.communicate()
		prsrec = output.decode('utf-8').splitlines()
		grbit = pygrib.open(filepath)
		#get surface stuff
		self.grdHgt = np.asfortranarray(grbit[int([s for s in prsrec if 'HGT:surface' in s][0].split(':')[0])].values)
		self.lat, self.lon = grbit[int([s for s in prsrec if 'HGT:surface' in s][0].split(':')[0])].latlons()
		self.twoMtrDpt = np.asfortranarray(grbit[int([s for s in prsrec if 'DPT:2 m above ground' in s][0].split(':')[0])].values)
		self.twoMtrTmp = np.asfortranarray(grbit[int([s for s in prsrec if 'TMP:2 m above ground' in s][0].split(':')[0])].values)
		self.grdPrs = np.asfortranarray(grbit[int([s for s in prsrec if 'PRES:surface' in s][0].split(':')[0])].values)
		self.mslp = np.asfortranarray(grbit[int([s for s in prsrec if 'MSLMA:mean sea level' in s][0].split(':')[0])].values)
		self.ugrd10m = np.asfortranarray(grbit[int([s for s in prsrec if 'UGRD:10 m above ground' in s][0].split(':')[0])].values)
		self.vgrd10m = np.asfortranarray(grbit[int([s for s in prsrec if 'VGRD:10 m above ground' in s][0].split(':')[0])].values)
		self.ushr6 = np.asfortranarray(grbit[int([s for s in prsrec if 'VUCSH:0-6000 m above ground' in s][0].split(':')[0])].values)
		self.vshr6 = np.asfortranarray(grbit[int([s for s in prsrec if 'VVCSH:0-6000 m above ground' in s][0].split(':')[0])].values)
		self.maxCape = np.asfortranarray(grbit[int([s for s in prsrec if 'CAPE:255-0 mb above ground' in s][0].split(':')[0])].values)      #useful to shorten calculations in cape3
		self.sbcape3 = np.asfortranarray(np.zeros(self.maxCape.shape))     #create base array for 3km cape, fills with 0s as base
		self.mlcape3 = np.asfortranarray(np.zeros(self.maxCape.shape))     #create base array for 3km cape, fills with 0s as base
		self.lsi = np.asfortranarray(np.full(self.maxCape.shape,99))     #create base array for lid strenght index, fills with 99s since 0 is a good value
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
			print("Level "+str(n))
			self.tmpH[n:]=np.asfortranarray(grbit[int([s for s in prsrec if 'TMP:'+str(n+1)+' hybrid level' in s][0].split(':')[0])].values)
			self.spfhH[n:]=np.asfortranarray(grbit[int([s for s in prsrec if 'SPFH:'+str(n+1)+' hybrid level' in s][0].split(':')[0])].values)
			self.hgtH[n:]=np.asfortranarray(grbit[int([s for s in prsrec if 'HGT:'+str(n+1)+' hybrid level' in s][0].split(':')[0])].values)
			self.prsH[n:]=np.asfortranarray(grbit[int([s for s in prsrec if 'PRES:'+str(n+1)+' hybrid level' in s][0].split(':')[0])].values)
			n = n+1
		


	
	

def makeMap(var, typev, ubrb, vbrb, lats, lons, title, minl, maxl, cmap, tstamp):
		
		#minl is levels interval if type is cont

	if typev == 'filled':

		#hi res
		#plt.figure(figsize=(48, 32), dpi=256,frameon=False)
		fig = plt.figure(figsize=(48, 32), dpi=256,frameon=False)
		ax = fig.gca()
		ax.axis('off')
		ax.set_xlim([0, 1])
		ax.set_ylim([0, 1])
		plt.subplots_adjust(left=0, right=1, top=1, bottom=0)
		m = Basemap(epsg=3857,llcrnrlon=(360-84.375),llcrnrlat=40.979898069620134,urcrnrlon=(360-67.5),urcrnrlat=48.922499263758255,resolution='i')
		x, y = m(lons, lats)
		cs = m.contourf(x,y,var,maxl-minl,cmap=cmap,vmin=minl,vmax=maxl)
		plt.savefig('/home/meteo/nowcast/imgs/nowcast_'+tstamp+'/'+title+'01hr.png', dpi=256, transparent=True)
		plt.close()	
		#del fig
		
		#med res
		#plt.figure(figsize=(12, 8), dpi=256,frameon=False)
		fig = plt.figure(figsize=(12, 8), dpi=256,frameon=False)
		ax = fig.gca()
		ax.axis('off')
		ax.set_xlim([0, 1])
		ax.set_ylim([0, 1])
		plt.subplots_adjust(left=0, right=1, top=1, bottom=0)
		m = Basemap(epsg=3857,llcrnrlon=(360-84.375),llcrnrlat=40.979898069620134,urcrnrlon=(360-67.5),urcrnrlat=48.922499263758255,resolution='i')
		x, y = m(lons, lats)
		cs = m.contourf(x,y,var,maxl-minl,cmap=cmap,vmin=minl,vmax=maxl)
		plt.savefig('/home/meteo/nowcast/imgs/nowcast_'+tstamp+'/'+title+'01mr.png', dpi=256, transparent=True)
		plt.close()
	
	if typev == 'cont':
		
		
		nlvl = int(np.amax(var)-np.amin(var))
		print(nlvl)
		var = gaussian_filter(var,1)
		
		#hi res
		fig = plt.figure(figsize=(48, 32), dpi=256,frameon=False)
		ax = fig.gca()
		ax.axis('off')
		ax.set_xlim([0, 1])
		ax.set_ylim([0, 1])
		plt.subplots_adjust(left=0, right=1, top=1, bottom=0)
		m = Basemap(epsg=3857,llcrnrlon=(360-84.375),llcrnrlat=40.979898069620134,urcrnrlon=(360-67.5),urcrnrlat=48.922499263758255,resolution='i')
		x, y =m(lons, lats)
		#udat, vdat, xv, yv = m.transform_vector(ubrb,vbrb,lons,lats,20,20,returnxy=True)

		cs = m.contour(x,y,var,nlvl*4,vmin=minl,vmax=maxl,colors=cmap,lindewidth=0.2)
		plt.clabel(cs, inline=1, fontsize=8, fmt='%1.1f')
		plt.savefig('/home/meteo/nowcast/imgs/nowcast_'+tstamp+'/'+title+'01hr.png', dpi=256, transparent=True)
		plt.close()
		
		#med res
		fig = plt.figure(figsize=(12, 8), dpi=256,frameon=False)
		ax = fig.gca()
		ax.axis('off')
		ax.set_xlim([0, 1])
		ax.set_ylim([0, 1])
		plt.subplots_adjust(left=0, right=1, top=1, bottom=0)
		m = Basemap(epsg=3857,llcrnrlon=(360-84.375),llcrnrlat=40.979898069620134,urcrnrlon=(360-67.5),urcrnrlat=48.922499263758255,resolution='i')
		x, y =m(lons, lats)
		#udat, vdat, xv, yv = m.transform_vector(ubrb,vbrb,lons,lats,20,20,returnxy=True)
		cs = m.contour(x,y,var,nlvl*2,vmin=minl,vmax=maxl,colors='black')
		plt.clabel(cs, inline=1, fontsize=8, fmt='%1.1f')
		plt.savefig('/home/meteo/nowcast/imgs/nowcast_'+tstamp+'/'+title+'01mr.png', dpi=256, transparent=True)
		plt.close()
	
	
	if typev == 'barb':
		#hi res
		fig = plt.figure(figsize=(48, 32), dpi=256,frameon=False)
		ax = fig.gca()
		ax.axis('off')
		ax.set_xlim([0, 1])
		ax.set_ylim([0, 1])
		plt.subplots_adjust(left=0, right=1, top=1, bottom=0)
		m = Basemap(epsg=3857,llcrnrlon=(360-84.375),llcrnrlat=40.979898069620134,urcrnrlon=(360-67.5),urcrnrlat=48.922499263758255,resolution='i')
		x, y =m(lons, lats)
		#udat, vdat, xv, yv = m.transform_vector(ubrb,vbrb,lons,lats,20,20,returnxy=True)
	
		#cs = m.barbs(x[::5,::5],y[::5,::5],ubrb[::5,::5],vbrb[::5,::5])
		cs = m.barbs(x,y,ubrb,vbrb,length=3.5,linewidth=0.2,sizes=dict(height=0.6))
		plt.savefig('/home/meteo/nowcast/imgs/nowcast_'+tstamp+'/'+title+'01hr.png', dpi=256, transparent=True)
		plt.close()
		
		#med res
		fig = plt.figure(figsize=(12, 8), dpi=256,frameon=False)
		ax = fig.gca()
		ax.axis('off')
		ax.set_xlim([0, 1])
		ax.set_ylim([0, 1])
		plt.subplots_adjust(left=0, right=1, top=1, bottom=0)
		m = Basemap(epsg=3857,llcrnrlon=(360-84.375),llcrnrlat=40.979898069620134,urcrnrlon=(360-67.5),urcrnrlat=48.922499263758255,resolution='i')
		x, y =m(lons, lats)
		#udat, vdat, xv, yv = m.transform_vector(ubrb,vbrb,lons,lats,20,20,returnxy=True)
	
		cs = m.barbs(x[::4,::4],y[::4,::4],ubrb[::4,::4],vbrb[::4,::4],length=3.5,linewidth=0.2,sizes=dict(height=0.6))
		#cs = m.barbs(x,y,ubrb,vbrb,length=3.5,linewidth=0.2,sizes=dict(height=0.6))
		plt.savefig('/home/meteo/nowcast/imgs/nowcast_'+tstamp+'/'+title+'01mr.png', dpi=256, transparent=True)
		plt.close()
	
	
	#try:
		#cc = m.contour(x,y,contourvar, colors=('gray'))
	#except:
		#pass
	#br = m.barbs(xv,yv,udat,vdat)
	
	#m.drawcoastlines()
	#m.drawstates()
	#m.drawcountries()
	
	#br = m.barbs(x[::5,::5],y[::5,::5],ubrb[::5,::5],vbrb[::5,::5])
	
	#norm = mpl.colors.Normalize(vmin=minl, vmax=maxl)
	
	#plt.pcolor(x, y, var, cmap=cmap, vmin=minl, vmax=maxl)

	#cbar = plt.colorbar(ticks=[0,25,50,100,150,200])
	#cbar = m.colorbar(location='bottom',pad="5%",ticks=[0,25,50,100,150,200])
	#cbar.ax.tick_params(labelsize=50)
	#cbar.set_label('J/kg*m3',size=50)
	#plt.title(title,size=50)
	#plt.savefig('/home/meteo/capeimgs/'+title+'.png')
	#plt.close()	
			
		

def colrMp(prod):
	
	if (prod == '3cape'):
		cdict = {'red':   ((0.0, 1.0, 1.0),
			 (0.25, 0.26, 0.26),
			 (0.5, 1.0, 1.0),
			(1.0, 1.0, 1.0)),
			
			'green': ((0.0, 1.0, 1.0),
			(0.25, 0.63, 0.63),
			(0.5, 1.0, 1.0),
			(0.75, 0.26, 0.26),
			(1.0, 0.54, 0.54)),
			
			'blue':  ((0.0, 1.0, 1.0),
			(0.25, 0.63, 0.63),
			(0.5, 0.0, 0.0),
			(0.75, 0.0, 0.0),
			(1.0, 1.0, 1.0)),
			
			'alpha': ((0.0,0.0,0.0),
			(0.25,1.0,1.0),
			(1.0,1.0,1.0))
			}
	if (prod == '2mtmp'):
		cdict = {'red': ((0.0,1,1),
			(0.275,0.725,0.216),
			(0.5,0.431,0),
			(0.6875,0.96,0.96),
			(0.875,0.92,0.51),
			(1.0,1,1)),
			
			'green': ((0.0,1,1),
			(0.275,0.314,0.333),
			(0.5,0.803,0.745),
			(0.6875,0.922,0.922),
			(0.875,0.118,0),
			(1.0,1,1)),
			
			'blue': ((0.0,1,1),
			(0.275,0.627,0.647),
			(0.5,0.862,0.0),
			(0.6875,0.059,0.059),
			(0.875,0.137,0),
			(1.0,1,1))}
	cmap = LinearSegmentedColormap('3cape',cdict)
	return cmap
	


	
path = sys.argv[1]
pathout = sys.argv[2]
tstamp = sys.argv[3]

print('Creating profile base object')
prof = profObjIdx(path,'hrrr')

print('Done creating profile object')
mlcape3 = prof.mlcape3
sbcape3 = prof.sbcape3
lsi = prof.lsi

print(prof.maxCape.shape)
print(prof.xmax)
print(prof.ymax)


cape3, lsi = cape3.cape3.severe(prof.maxCape,prof.tmpH,prof.spfhH,prof.prsH,prof.hgtH,prof.grdHgt)
	


print('Printing products')

#makeMap(mltmp,prof.lat,prof.lon,'MLTMP',[250,255,260,265,270,275,280,285,290,295,300,305,310,315])

print(np.amax(cape3))

cmap = colrMp('3cape')


makeMap(cape3,'filled',0,prof.ugrd10m,prof.lat,prof.lon,'MLCAPE3',25,200,cmap,tstamp)

cmap = colrMp('2mtmp')

prof.twoMtrTmp = prof.twoMtrTmp -273.15

makeMap(prof.twoMtrTmp,'filled',0,prof.ugrd10m,prof.lat,prof.lon,'2MTMP',-40,40,cmap,tstamp)


makeMap(prof.twoMtrTmp,'barb',prof.ugrd10m*1.944,prof.vgrd10m*1.944,prof.lat,prof.lon,'10wbrb',-40,40,cmap,tstamp)


mslp = np.round(prof.mslp*0.01,1)
print(mslp)

makeMap(mslp,'cont',4,prof.vgrd10m*1.944,prof.lat,prof.lon,'mslp',800,1200,'black',tstamp)


#makeMap(lsi,prof.lat,prof.lon,'SBCAPE3',[0,1,2,3,5,7])


