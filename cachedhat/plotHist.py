#!/usr/bin/python3

import sys
import os
import matplotlib.pyplot as plt
import numpy as np

def main():
    # print command line arguments
    if len(sys.argv) <= 2:
        print('usage:\n./plotHist.py heapProfileHist.out heatmap_folder')
        exit()
    filename = sys.argv[1]
    outFolder = sys.argv[2]
    os.mkdir(outFolder)

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
                if mat == None:
                    mat = np.array(col)
                else:
                    mat = np.vstack([mat, np.array(col)])
            line = fp.readline()
            cnt += 1
    # plot heatmap
    # a = np.random.random((16, 16))
    # print(type(a))
    # plt.imshow(a, cmap='hot', interpolation='nearest')
    # plt.show()

if __name__ == "__main__":
    main()