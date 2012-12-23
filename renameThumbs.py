#!/usr/bin/python

import sys
import os
import os.path


class Entry(object):

    def __init__(self, path, directory):
        self.empty = path == ""
        self.path_raw = path
        self.directory = directory
        if directory == -1:
            self.path = path.replace("\\", "/")
        else:
            self.path = path

    def is_dir(self):
        return self.directory == -1

    def is_file(self):
        return not self.is_dir() and not self.empty

    def __str__(self):
        return "Entry(path='%s', directory=%d, empty=%s)" % (
            self.path, self.directory, str(self.empty))


def parseIndex(ifile):
    db = []
    for i, line in enumerate(ifile.readlines()):
        columns = line.split("\t")
        path = columns[0]
        directory = int(columns[7])
        e = Entry(path, directory)
        db.append(e)
    for e in db:
        if e.is_file():
            e.path = os.path.join(db[e.directory].path, e.path)
    return db


def move(src, dst):
    if not os.path.exists(src):
        print "skip", src
        return
    dstdir = os.path.dirname(dst)
    if os.path.exists(dstdir):
        assert os.path.isdir(dstdir)
    else:
        os.makedirs(dstdir)
    print "mv %s %s" % (src, dst)
    os.rename(src, dst)


def renameWithIndex(index, srcdir, dstdir):
    for i, e in enumerate(index):
        if e.is_file():
            move("%s/%010d.jpg"%(srcdir, i), "%s/%s"%(dstdir,e.path))


def main():
    indexfile = sys.argv[1]
    srcdir = sys.argv[2]
    dstdir = sys.argv[3]
    index = parseIndex(open(indexfile))
    renameWithIndex(index, srcdir, dstdir)


if __name__ == "__main__":
    main()
