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
from matplotlib.colors import LinearSegmentedColormap
from scipy.ndimage.filters import gaussian_filter

def makeMap(var, typev, ubrb, vbrb, lats, lons, title, minl, maxl, cmp, tstamp):
		
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
		print('Init base map')
		m = Basemap(epsg=3857,llcrnrlon=(360-84.375),llcrnrlat=40.979898069620134,urcrnrlon=(360-67.5),urcrnrlat=48.922499263758255,resolution='i')
		print('latlon to xy')
		x, y = m(lons, lats)
		print('drawing contours')
		#cs = m.contourf(x,y,var)
		cs = m.imshow(x,y,var,cmap=cmp,vmin=minl,vmax=maxl,interpolation='bilinear')
		print('savingfig')
		plt.savefig('/home/meteo/html/'+title+'01hr.png', dpi=256, transparent=True)
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
		cs = m.contourf(x,y,var,maxl-minl,cmap=cmp,vmin=minl,vmax=maxl)
		plt.savefig('/home/meteo/html/'+title+'01mr.png', dpi=256, transparent=True)
		plt.close()

		
def colrMp(prod):
	
	if (prod == 'rad'):
		cdict = {'red':   ((0.0, 1.0, 1.0),
			 (0.05, 0.114, 0.114),
			 (0.175, 0.349, 0.349),
			 (0.225, 0.129, 0.129),
			 (0.325, 0.02, 0.02),
			 (0.375, 0.984, 0.78),
			 (0.425, 0.992, 0.674),
			 (0.50, 0.992, 0.529),
			 (0.60, 0.757, 0.784),
			 (0.70, 0.647, 0.251),
			 (0.75, 0.529, 0.212),
			 (0.80, 0.678, 0.678),
			 (0.85, 0.412, 0.412),
			 (0.95, 0, 0),
			(1.0, 0, 0)),
			
			'green': ((0.0, 1.0, 1.0),
			(0.05, 0.145, 0.145),
			 (0.175, 0.608, 0.608),
			 (0.225, 0.729, 0.729),
			 (0.325, 0.396, 0.396),
			 (0.375, 0.988, 0.69),
			 (0.425, 0.584, 0.361),
			 (0.50, 0.149, 0.169),
			 (0.60, 0.580, 0.09),
			 (0.70, 0.008, 0.0),
			 (0.75, 1.0, 0.471),
			 (0.80, 0.388, 0.388),
			 (0.85, 0.0, 0.0),
			 (0.95, 0, 0),
			(1.0, 0, 0)),
			
			'blue':  ((0.0, 1.0, 1.0),
			(0.05, 0.235, 0.235),
			 (0.175, 0.671, 0.671),
			 (0.225, 0.282, 0.282),
			 (0.325, 1.0, 1.0),
			 (0.375, 0.0, 0.0),
			 (0.425, 0.008, 0.008),
			 (0.50, 0.0, 0.086),
			 (0.60, 0.702, 0.467),
			 (0.70, 0.843, 0.573),
			 (0.75, 0.992, 0.557),
			 (0.80, 0.251, 0.251),
			 (0.85, 0.016, 0.016),
			 (0.95, 0, 0),
			(1.0, 0, 0)),
			
			'alpha': ((0.0,0.0,0.0),
			(0.05,0.0,0.0),
			(0.175,1.0,1.0),
			(1.0,1.0,1.0))
			}
	cmap = LinearSegmentedColormap('3cape',cdict)
	return cmap


print('Opening grib file')
hsr = pygrib.open('MRMS_SeamlessHSR.latest.grib2')
print('Setting lats and lon')
lat, lon = hsr[1].latlons()
print('setting value')
rad = hsr[1].values
print(rad.shape)
print('Making map')

cmap = colrMp('rad')

makeMap(rad,'filled','x','x',lat,lon,'MRMS_Composite',0,100,cmap,'0000')

    
