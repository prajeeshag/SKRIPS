
from ecmwfapi import ECMWFService
import typer

MAXMEM=25

app = typer.Typer()



@app.command()
def download(date: str, dtype: str, endhour: int = 5160, nmem: int = 25, test: bool = False):

    server = ECMWFService("mars")

    minStarth=24
    maxEndh=endhour
    endh=min(5160,maxEndh)



    if dtype=='pl':
        starth=max(12,minStarth)
        inth=12
        step='/'.join(map(str,list(range(starth,endh+inth,inth))))
        if test:
            print(step)
        else:
            for i in range(2):
                if i==1:
                    if nmem <= 13:
                        break
                    number="13/14/15/16/17/18/19/20/21/22/23/24"
                    nnmem=min(MAXMEM,nmem)
                    number="/".join(map(str,list(range(13,nnmem))))
                else:
                    number="0/1/2/3/4/5/6/7/8/9/10/11/12"
                    nnmem=min(13,nmem)
                    number="/".join(map(str,list(range(0,nnmem))))

                server.execute({
                    "class": "c3",
                    "date": date,
                    "expver": "1",
                    "levelist": "10/30/50/100/200/300/400/500/700/850/925/1000",
                    "levtype": "pl",
                    "method": "1",
                    "number": number,
                    "origin": "ecmf",
                    "param": "129.128/130.128/131.128/132.128/133.128",
                    "step": step,
                    "stream": "mmsf",
                    "system": "51",
                    "time": "00:00:00",
                    "type": "fc",
                    },f"pl_{date}_{i}.grib"
                )

    elif dtype=='sfc':
        starth=max(0,minStarth)
        inth=6
        step='/'.join(map(str,list(range(starth,endh+inth,inth))))
        nnmem=min(MAXMEM,nmem)
        number="/".join(map(str,list(range(0,nnmem))))

        if test:
            print(step)
        else:
            server.execute({
                "class":"c3",
                "date":date,
                "expver":"1",
                "levtype":"sfc",
                "method":"1",
                "number":number,
                "origin":"ecmf",
                #"param":"34.128/129.128/139.128/151.128/165.128/166.128/167.128/168.128/172.128",
                "param":"34.128/139.128/151.128/165.128/166.128/167.128/168.128",
                "step": step,
                "stream":"mmsf",
                "system":"51",
                "time":"00:00:00",
                "type":"fc",
                },f"sfc_{date}.grib"
            )

    elif dtype=='soil':

        starth=max(24,minStarth)
        inth=24
        step='/'.join(map(str,list(range(starth,endh+inth,inth))))
        nnmem=min(MAXMEM,nmem)
        number="/".join(map(str,list(range(0,nnmem))))

        if test:
            print(step)
        else:
            server.execute({
                "class":"c3",
                "date":date,
                "expver":"1",
                "levelist":"1/2/3/4",
                "levtype":"sol",
                "method":"1",
                "number": number,
                "origin":"ecmf",
                "param":"260199",
                "step": step,
                "stream":"mmsf",
                "system":"51",
                "time":"00:00:00",
                "type":"fc",
                },f"soil_{date}.grib"
            )


if __name__ == "__main__":
    app()
