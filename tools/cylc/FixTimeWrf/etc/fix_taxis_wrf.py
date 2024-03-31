import os
from pathlib import Path
import typer
from multiprocessing import Pool
from cdo import Cdo
from nco import Nco
from nco.custom import Limit, LimitSingle

from typing_extensions import Annotated

cdo = Cdo()
nco = Nco()


def mdmean(fil):
    parent = fil.parent.absolute()
    ofpath= parent / 'tmp' / fil.name
    ifil = fil.as_posix()
    ofil = ofpath.as_posix()
    opt = [
        Limit("Time",0,3)
    ]
    
    ntimes = cdo.ntime(input=ifil)


    if ofpath.exists():
        return f'{ofil} exists'

    if int(ntimes[0]) == 5:
        nco.ncks(input=ifil,output=ofil,options=opt)
        return f'{ifil} done'
    else:
        return f'{ifil} nothing need to be done'




def make_monthly(
    path: Annotated[
        Path,
        typer.Option(
            exists=True,
            file_okay=False,
            dir_okay=True,
            writable=True,
            readable=True,
        ),
    ],
    file_glob: Annotated[str, typer.Option()],
    num_procs: Annotated[int, typer.Option()]=8
):
    infiles = path.glob(file_glob)
    outdir = path / "tmp" 

    outdir.mkdir(parents=True, exist_ok=True)

    parameters_list=[(fil,) for fil in infiles] 
    
    if num_procs > len(parameters_list):
        num_procs = len(parameters_list)

    with Pool(processes=num_procs) as pool:
       results = pool.starmap(mdmean, parameters_list)

    print(results)


if __name__ == "__main__":
    typer.run(make_monthly)
