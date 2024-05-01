from xmitgcm import open_mdsdataset
import os
import glob
from datetime import datetime, timedelta
import typer
import shutil


app = typer.Typer()

def mv_files_monthly(year, month, refDate, varName):
    ref_date = datetime.strptime(f'{refDate} 00:00:00', '%Y-%m-%d %H:%M:%S')
    matching_files = glob.glob(f'{varName}.*.??ta')
    mon_dir=f'{year}/{month}/{varName}'
    os.makedirs(mon_dir, exist_ok=True)
    for f in matching_files:
        if if_matching_month(year,month,ref_date,f):
            fo = os.path.join(mon_dir,f)
            os.rename(f,fo)
    return mon_dir 

        
def if_matching_month(year, month, ref_date, fName):
    timestamp = get_timestamp(fName)
    time_change = timedelta(minutes=int(timestamp))
    date = ref_date + time_change
    return year == date.strftime("%Y") and month == date.strftime("%m")


def get_timestamp(fileName):
    wlist = fileName.split('.')
    return wlist[-2]
    
 
def write_to_nc(varName, refDate, deltaT, year, month, out_dir, gridpath, delete):

    mon_dir = mv_files_monthly(year, month, refDate, varName)

    print(f'moved files {varName} {refDate}')

    ref_date = f"{refDate} 0:0:0"

    # Path to the mitgcm run directory where all
    # *.data and *.meta files are present
    complevel = 2  # compression level
    shuffle = True
    zlib = True

    matching_files = glob.glob(f'{mon_dir}/*.data')
    if not matching_files:
        return

    ds = open_mdsdataset(
        mon_dir,
        grid_dir=gridpath,
        geometry='curvilinear',
        ignore_unknown_vars=True,
        delta_t=deltaT,
        ref_date=ref_date,
        prefix=[varName,],
    )

    fname_suffix = f'{year}_{month}.nc'
    
    var = ds[varName]
    encode = {
        varName: {
            'zlib': zlib,
            'complevel': complevel,
            'shuffle': shuffle
        }
    }
    out_file = f'{out_dir}/{varName}_{fname_suffix}'
    var.to_netcdf(out_file, encoding=encode)
    print(f'Wrote file {out_file}')
    if delete:
        shutil.rmtree(mon_dir)


@app.command()
def to_nc(refdate: str, dt: int, year: str, month: str, outdir: str, gridpath: str, delete: bool = False):

    varNames = [
        'Convtave', 'ETAtave', 'Eta', 'Eta2tave', 'PH',
        'PHL', 'PHL2tave', 'PHLtave', 'PhHytave', 'S',
        'Stave', 'T', 'TTtave', 'Tdiftave', 'Ttave',
        'U', 'UStave', 'UTtave', 'UUtave', 'UVtave',
        'V', 'VStave', 'VTtave', 'VVtave', 'W', 'WStave',
        'WTtave', 'sFluxtave', 'tFluxtave', 'uFluxtave',
        'uVeltave', 'vFluxtave', 'vVeltave', 'wVeltave',
    ]
      
    os.makedirs(outdir, exist_ok=True)
    for varName in varNames:
      write_to_nc(varName, refdate, dt, year, month, outdir, gridpath, delete)


if __name__ == "__main__":
    app()




