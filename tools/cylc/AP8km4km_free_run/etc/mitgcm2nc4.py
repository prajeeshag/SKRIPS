from xmitgcm import open_mdsdataset
import os
import glob


def write_to_nc(varName):
    deltaT = $deltaT  # noqa
    ref_date = "$ref_date 0:0:0"

    # Path to the mitgcm run directory where all
    # *.data and *.meta files are present
    complevel = 0  # compression level
    shuffle = False
    zlib = False
    matching_files = glob.glob(f'{varName}.*.data')
    if not matching_files:
        return

    ds = open_mdsdataset(
        "./",
        ignore_unknown_vars=True,
        delta_t=deltaT,
        ref_date=ref_date,
        prefix=[varName,],
    )
    timeStamp1 = ds.time[0].dt.strftime("%Y%m%d%H%M%S").item()
    timeStamp2 = ds.time[-1].dt.strftime("%Y%m%d%H%M%S").item()
    fname_suffix = f'{timeStamp1}-{timeStamp2}.nc'
    
    var = ds[varName]
    encode = {
        varName: {
            'zlib': zlib,
            'complevel': complevel,
            'shuffle': shuffle
        }
    }
    out_file = f'{varName}_{fname_suffix}'
    var.to_netcdf(out_file, encoding=encode)
    print(f'Wrote file {out_file}')


varNames = [
    'Convtave',
    'ETAtave',
    'Eta',
    'Eta2tave',
    'PH',
    'PHL',
    'PHL2tave',
    'PHLtave',
    'PhHytave',
    'S',
    'Stave',
    'T',
    'TTtave',
    'Tdiftave',
    'Ttave',
    'U',
    'UStave',
    'UTtave',
    'UUtave',
    'UVtave',
    'V',
    'VStave',
    'VTtave',
    'VVtave',
    'W',
    'WStave',
    'WTtave',
    'sFluxtave',
    'tFluxtave',
    'uFluxtave',
    'uVeltave',
    'vFluxtave',
    'vVeltave',
    'wVeltave',
]

for varName in varNames:
    write_to_nc(varName)

