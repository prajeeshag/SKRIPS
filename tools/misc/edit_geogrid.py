import os

import xarray as xr
import numpy as np
import matplotlib.pyplot as plt

nx=800
ny=780
gridRatio=2
geoFile='geo_em.d01.nc'
bathyFile='bathymetry.bin'

def rd_ocnMask(imask,gridRatio):
    if gridRatio < 2:
        return imask
    ny, nx = imask.shape
    ny1, nx1 = ny//2, nx//2
    omask = np.zeros([ny1,nx1])
    for j in range(ny1):
        for i in range(nx1):
            jstart=int(gridRatio*j); jend=jstart+gridRatio
            istart=int(gridRatio*i); iend=istart+gridRatio
            n=np.sum(imask[jstart:jend,istart:iend])/(gridRatio*gridRatio)
            if n==1:
                omask[j,i] = 1
            elif n==0:
                omask[j,i] = 0
            else:
                omask[j,i] = -999
                raise ValueError(f'Cannot group ocnMask for points [{jstart,jend,istart,iend}]')
    return omask


def circle_search(arr,x,y):
    ny, nx = arr.shape
    R = max(nx-1,ny-1)
    for r in range(R):
        isc, iec = max(x - r, 0), min(x + r, nx-1)
        jsc, jec = max(y - r, 0), min(y + r, ny-1)

        #Left-Right
        for j in range(jsc,jec+1):
            if arr[j,isc] == 1:
                return j, isc
            if arr[j,iec] == 1:
                return j, iec

        #Left-Right
        for i in range(isc,iec+1):
            if arr[jsc,i] == 1:
                return jsc, i
            if arr[jec,i] == 1:
                return jec, i
    return None


def circ_search(arr,coords):
    nnmap = {}
    for y, x in coords:
        res = circle_search(arr,x,y)
        if not res:
            raise ValueError(f'Could not find a nn map for point ({y},{x})')
        nnmap[(y,x)] = res
    return nnmap

geoDs=xr.open_dataset(geoFile)
bathy=np.fromfile(bathyFile, ">f4").reshape(ny, nx)

LANDMASK=geoDs['LANDMASK'] # 1=land, 0=water
LANDUSEF=geoDs['LANDUSEF']
LU_INDEX=geoDs['LU_INDEX']
HGT_M=geoDs['HGT_M']
SOILTEMP=geoDs['SOILTEMP']
SOILCTOP=geoDs['SOILCTOP']
SOILCBOT=geoDs['SOILCBOT']
SCB_DOM=geoDs['SCB_DOM'] 
SCT_DOM=geoDs['SCT_DOM'] 
ALBEDO12M=geoDs['ALBEDO12M']
GREENFRAC=geoDs['GREENFRAC']
LAI12M=geoDs['LAI12M']
SNOALB=geoDs['SNOALB']

ocnMask=np.where(bathy<0,0,1) # 1-land, 0-water
ocnMaskR=rd_ocnMask(ocnMask,gridRatio)

mmPoints=ocnMaskR-LANDMASK.values.squeeze()

plt.pcolormesh(ocnMaskR)
plt.colorbar()
plt.savefig('ocnMaskR.png',dpi=1200)
plt.close()

plt.pcolormesh(mmPoints)
plt.colorbar()
plt.savefig('mmPoints.png',dpi=1200)
plt.close()

_,nland_cat,_,_ = LANDUSEF.shape

#for lc in range(nland_cat):
#    var = LANDUSEF.values[0,lc,:,:]
#    plt.pcolormesh(var)
#    plt.colorbar()
#    plt.savefig(f'landusef_{lc+1}.png',dpi=1200)
#    plt.close()

var = LU_INDEX.squeeze()
lndMask=np.where(var==17,0,1) # 1-land, 0-water

mmPointsO=ocnMaskR-lndMask
plt.pcolormesh(mmPointsO)
plt.colorbar()
plt.savefig('mmPointsO.png',dpi=1200)
plt.close()

toLndIdx=np.where(mmPointsO==1)
toOcnIdx=np.where(mmPointsO==-1)
x = toLndIdx
toLndIdxL = [(x[0][n],x[1][n]) for n in range(len(x[0]))]
x = toOcnIdx
toOcnIdxL = [(x[0][n],x[1][n]) for n in range(len(x[0]))]

file = open('toOcn.txt', 'w')
for (j,i) in toOcnIdxL:
    file.write(f'({j},{i}) \n')
    LANDMASK.values[:,j,i] = 0
    LU_INDEX.values[:,j,i] = 17
    LANDUSEF.values[:,0,j,i] = 0.
    LANDUSEF.values[:,16,j,i] = 1.
    HGT_M.values[:,j,i] = 0.
file.close()

var = LU_INDEX.squeeze()
lndMask=np.where(var==17,0,1) # 1-land, 0-water


toLndMap = circ_search(lndMask,toLndIdxL)

# Open the file
file = open('toLndMap.txt', 'w')
for ((sj,si),(dj,di)) in toLndMap.items():
    file.write(f'({sj},{si}) => ({dj},{di})\n')
    LANDMASK[:,sj,si]  = LANDMASK[:,dj,di]
    LANDUSEF[:,:,sj,si]  = LANDUSEF[:,:,dj,di]
    LU_INDEX[:,sj,si]  = LU_INDEX[:,dj,di]
    HGT_M[:,sj,si]     = HGT_M[:,dj,di]
    SOILTEMP[:,sj,si]  = SOILTEMP[:,dj,di]
    SOILCTOP[:,:,sj,si]  = SOILCTOP[:,:,dj,di]
    SOILCBOT[:,:,sj,si]  = SOILCBOT[:,:,dj,di]
    SCB_DOM[:,sj,si]   = SCB_DOM[:,dj,di]
    SCT_DOM[:,sj,si]   = SCT_DOM[:,dj,di]
    ALBEDO12M[:,:,sj,si] = ALBEDO12M[:,:,dj,di]
    GREENFRAC[:,:,sj,si] = GREENFRAC[:,:,dj,di]
    LAI12M[:,:,sj,si]    = LAI12M[:,:,dj,di]
    SNOALB[:,sj,si]    = SNOALB[:,dj,di]

file.close()


var = LU_INDEX.squeeze()
lndMask=np.where(var==17,0,1) # 1-land, 0-water
mmPointsO=ocnMaskR-lndMask
print(np.where(mmPointsO==1.0))
plt.pcolormesh(mmPointsO)
plt.colorbar()
plt.savefig('mmPointsO1.png',dpi=1200)
plt.close()

geoFileP, geoFileExt = os.path.splitext(geoFile)
oGeoFile=f'{geoFileP}_mod{geoFileExt}'

geoDs['LANDMASK'].values  = LANDMASK.values
geoDs['LANDUSEF'].values  = LANDUSEF.values
geoDs['LU_INDEX'].values  = LU_INDEX.values
geoDs['HGT_M'].values     = HGT_M.values
geoDs['SOILTEMP'].values  = SOILTEMP.values
geoDs['SOILCTOP'].values  = SOILCTOP.values
geoDs['SOILCBOT'].values  = SOILCBOT.values
geoDs['SCB_DOM'].values   = SCB_DOM.values
geoDs['SCT_DOM'].values   = SCT_DOM.values  
geoDs['ALBEDO12M'].values = ALBEDO12M.values
geoDs['GREENFRAC'].values = GREENFRAC.values
geoDs['LAI12M'].values    = LAI12M.values
geoDs['SNOALB'].values    = SNOALB.values

encode = {}
for var in geoDs.data_vars:
    if var == "Times":
        encode[var] = {
            "char_dim_name": "DateStrLen",
            "zlib": True,
        }
        continue
    encode[var] = {"_FillValue": None, 'zlib': True, 'complevel':2, 'shuffle': True}
geoDs.to_netcdf(oGeoFile, format="NETCDF4", encoding=encode)
