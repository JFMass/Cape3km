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
    		cs = m.contourf(x,y,var)
		#cs = m.contourf(x,y,var,maxl-minl,cmap=cmap,vmin=minl,vmax=maxl)
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

hsr = pygrib.open('MRMS_SeamlessHSR.latest.grib2')
lat, lon = hsr[0].latlons()
rad = hsr[0].values
makeMap(rad,'filled','x','x',lat,lon,'MRMS_Composite',0,70,'xx','0000')

    
