#!/usr/bin/python3

import sys
import os
import errno
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np

def main():
    # print command line arguments
    if len(sys.argv) <= 2:
        print('usage:\n./plotHM.py heapProfileHM(R/W).out heatmap_folder')
        exit()
    filename = sys.argv[1]
    outFolder = sys.argv[2]
    try:
        os.mkdir(outFolder)
    except OSError as exc:
        if exc.errno != errno.EEXIST:
            raise
        pass
    ometaF = open(outFolder+"/meta.txt", "w")

    mat = None
    mat_name = None
    with open(filename) as fp:
        line = fp.readline()
        cnt = 1
        while line:
            if line.strip().find("res") != -1 or line.strip().find("size-limit") != -1:
                print(line)
                ometaF.write(line)
            elif line.strip(' ') == '\n':
                # do nothing, filter out
                if mat is not None:
                    plt.xlabel('mem addr')
                    plt.ylabel('time')
                    plt.imshow(mat, cmap='hot', interpolation='nearest')
                    # estimate locality, is this formula right?
                    avg_byts_per_bucket = np.sum(mat)/np.size(mat, 1)
                    total_0_axis = np.sum(mat, axis=0)
                    locality_0_aixs = np.absolute(total_0_axis - avg_byts_per_bucket)
                    # Or ?
                    # locality_0_aixs = total_0_axis - avg_byts_per_bucket * 0.95
                    # locality_0_aixs[locality_0_aixs < 0] = 0
                    est_locality = np.sum(locality_0_aixs)/np.sum(mat)
                    
                    plt.savefig(outFolder+"/"+mat_name+"_"+format(est_locality, '.3f')+".png", dpi=1000)
                    
            elif line.strip().find("fs") != -1:
                mat_name = line.strip()
                # create new array
                mat = None
            else:
                col = (line.strip('\t\n').replace("\t"," "))
                col = list(map(int, col.split()))
                col = np.ndarray((1,len(col)), buffer=np.array(col))
                if mat is None:
                    mat = col
                else:
                    if col.shape[1]> mat.shape[1]:
                        zero_mat = np.zeros((mat.shape[0], col.shape[1]-mat.shape[1]))
                        mat = np.concatenate([mat, zero_mat], axis=1)
                    elif col.shape[1] < mat.shape[1]:
                        pad_zeros = np.zeros(mat.shape[1] - col.shape[1])
                        pad_zeros = np.ndarray((1,len(pad_zeros)), buffer=pad_zeros)
                        col = np.concatenate([col, pad_zeros], axis=1)
                    else:
                        pass
                    mat = np.vstack([mat, col])
            line = fp.readline()
            cnt += 1
            if cnt % 1e3 == 0:
                print('[%d] 1k line pass' % (cnt/1e3))

    # handle the end of file
    if mat is not None:
        plt.xlabel('mem addr')
        plt.ylabel('time')
        plt.imshow(mat, cmap='hot', interpolation='nearest')
        # estimate locality, is this formula right?
        avg_byts_per_bucket = np.sum(mat)/np.size(mat, 1)
        total_0_axis = np.sum(mat, axis=0)
        locality_0_aixs = np.absolute(total_0_axis - avg_byts_per_bucket)
        # Or ?
        # locality_0_aixs = total_0_axis - avg_byts_per_bucket * 0.95
        # locality_0_aixs[locality_0_aixs < 0] = 0
        est_locality = np.sum(locality_0_aixs)/np.sum(mat)
        
        plt.savefig(outFolder+"/"+mat_name+"_"+format(est_locality, '.3f')+".png", dpi=1000)
    ometaF.close()
    # plot heatmap example
    # a = np.random.random((16, 16))
    # print(type(a))
    # plt.imshow(a, cmap='hot', interpolation='nearest')
    # plt.show()

if __name__ == "__main__":
    main()