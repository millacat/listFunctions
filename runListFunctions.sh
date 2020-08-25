fsharpc --sig:signatures.fsi listFunctions.fs     # create signature file (.fsi)
fsharpc -a signatures.fsi listFunctions.fs        # build a library (.dll)
fsharpc -r listFunctions.dll useListFunctions.fsx # compile the use of library fcts
mono useListFunctions.exe                         # run executable

