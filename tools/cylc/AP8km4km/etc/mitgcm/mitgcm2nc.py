from xmitgcm import open_mdsdataset
import os
import glob
from datetime import datetime, timedelta
import typer
import shutil

import concurrent.futures
from functools import partial

import warnings


app = typer.Typer()


def mv_files_monthly(year, month, refDate, varName):
    ref_date = datetime.strptime(f"{refDate} 00:00:00", "%Y-%m-%d %H:%M:%S")
    matching_files = glob.glob(f"{varName}.*.??ta")
    mon_dir = f"{year}/{month}/{varName}"
    os.makedirs(mon_dir, exist_ok=True)
    for f in matching_files:
        if if_matching_month(year, month, ref_date, f):
            fo = os.path.join(mon_dir, f)
            os.rename(f, fo)
    return mon_dir


def if_matching_month(year, month, ref_date, fName):
    timestamp = get_timestamp(fName)
    time_change = timedelta(minutes=int(timestamp))
    date = ref_date + time_change
    return year == date.strftime("%Y") and month == date.strftime("%m")


def get_timestamp(fileName):
    wlist = fileName.split(".")
    return wlist[-2]


def write_to_nc(varName, refDate, deltaT, year, month, out_dir, gridpath, delete):

    mon_dir = mv_files_monthly(year, month, refDate, varName)

    print(f"moved files {varName} {refDate}")

    ref_date = f"{refDate} 0:0:0"

    matching_files = glob.glob(f"{mon_dir}/*.data")
    if not matching_files:
        print(f"No files found for {varName}")
        return

    ds = open_mdsdataset(
        mon_dir,
        grid_dir=gridpath,
        geometry="curvilinear",
        ignore_unknown_vars=True,
        delta_t=deltaT,
        ref_date=ref_date,
        prefix=[
            varName,
        ],
    )
    print(f"Datasets opened for {varName}")

    fname_suffix = f"{year}_{month}.nc"

    var = ds[varName]

    var.load()

    print(f"test {varName}")

    encode = {
        varName: {
            "zlib": True,
            "complevel": 1,
            "shuffle": True,
            "fletcher32": True,
            "chunksizes": tuple(map(lambda x: x // 10, var.shape)),
        }
    }

    out_file = f"{out_dir}/{varName}_{fname_suffix}"
    print(f"Writing file {out_file}")

    var.to_netcdf(out_file, encoding=encode, format="NETCDF4", engine="netcdf4")
    print(f"Wrote file {out_file}")

    if delete:
        print(f"Deleting binary files")
        shutil.rmtree(mon_dir)


def write_to_nc_parallel(
    varNames, refdate, dt, year, month, outdir, gridpath, delete, max_threads
):

    partial_process_file = partial(
        write_to_nc,
        refDate=refdate,
        deltaT=dt,
        year=year,
        month=month,
        out_dir=outdir,
        gridpath=gridpath,
        delete=delete,
    )

    with concurrent.futures.ThreadPoolExecutor(max_workers=max_threads) as executor:
        executor.map(partial_process_file, varNames)


@app.command()
def to_nc(
    refdate: str,
    dt: int,
    year: str,
    month: str,
    outdir: str,
    gridpath: str,
    delete: bool = False,
):

    varNames = [
        "ETAN",
        "SALT",
        "THETA",
        "UVEL",
        "VVEL",
        "WVEL",
        "MXLDEPTH",
        "SST",
        "SSH",
    ]

    os.makedirs(outdir, exist_ok=True)

    max_threads = 4

    for varName in varNames:
        write_to_nc(varName, refdate, dt, year, month, outdir, gridpath, delete)


if __name__ == "__main__":
    app()
