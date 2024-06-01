


def read_file_to_set(filename):
    with open(filename, 'r') as file:
        return set(line.strip() for line in file)

# File paths
file1 = 'wrfout_upp.log'
file3 = 'wrfoutvars.log'
file2 = 'wrfout3dvars.log'
file4 = 'wrfout2dvars.log'

# Reading files and converting to sets
upp = read_file_to_set(file1)
wrf3d = read_file_to_set(file2)
wrf2d = read_file_to_set(file4)
wrfAll = read_file_to_set(file3)


allWantedbyUpp = wrfAll.intersection(upp)
upp3d = wrf3d.intersection(upp)
upp2d = wrf2d.intersection(upp)

upp2d.update(['ISLTYP',])
upp3d.update([
    'P_TOP',
    'TSK',
    'PSFC',
    'PBLH',
    'HGT',
    'SMOIS',
    'TSLB',
    'XLAT',
    'XLONG',
    ])

leftOver = list(allWantedbyUpp.difference(upp3d | upp2d))
unWanted = wrfAll.difference(upp)


print(f'Unlisted variables: {leftOver}')


# Open the file in write mode
with open('wrf_output.txt', 'w') as file:
    # Write each line to the file
    file.write('# 3D fields, also contains some 2D fields neccesary for UPP \n')
    for line in upp3d:
        file.write(f'+:h:5:{line}'+'\n')

    file.write('# 2D fields \n')
    for line in upp2d:
        file.write(f'+:h:6:{line}'+'\n')


