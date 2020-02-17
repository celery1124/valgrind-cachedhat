#!/usr/bin/python3

import sys
import os
import errno
import matplotlib.pyplot as plt
import numpy as np

def main():
    # print command line arguments
    if len(sys.argv) <= 2:
        print('usage:\n./plotHist.py heapProfileHist.out heatmap_folder')
        exit()
    filename = sys.argv[1]
    outFolder = sys.argv[2]
    try:
        os.mkdir(outFolder)
    except OSError as exc:
        if exc.errno != errno.EEXIST:
            raise
        pass

    mat = None
    mat_name = None
    with open(filename) as fp:
        line = fp.readline()
        cnt = 1
        while line:
            if line.strip(' ') == '\n':
                # do nothing, filter out
                if mat != None:
                    plt.xlabel('mem addr')
                    plt.ylabel('time')
                    plt.imshow(mat, cmap='hot', interpolation='nearest')
                    plt.savefig(outFolder+"/"+mat_name+".png", dpi=1000)
                    
            elif line.strip().find("fs") != -1:
                mat_name = line.strip()
                # create new array
                mat = None
            else:
                col = (line.strip('\t\n').replace("\t"," "))
                col = list(map(int, col.split()))
                col = np.array(col)
                if mat == None:
                    mat = col
                else:
                    if col.shape[0]> mat.shape[1]:
                        zero_mat = np.zeros((mat.shape[0], len(col)-mat.shape[1]))
                        mat = np.concatenate([mat, zero_mat], axis=1)
                    elif col.shape[0] < mat.shape[1]:
                        col = np.concatenate([col, np.zeros(mat.shape[1] - col.shape[0])])
                    else:
                        pass
                    mat = np.vstack([mat, col])
            line = fp.readline()
            cnt += 1
    # plot heatmap
    # a = np.random.random((16, 16))
    # print(type(a))
    # plt.imshow(a, cmap='hot', interpolation='nearest')
    # plt.show()

if __name__ == "__main__":
    main()