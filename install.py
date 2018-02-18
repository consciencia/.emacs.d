
#!/usr/bin/env python
# # -*- coding: utf-8 -*-
import os
import shutil
import zipfile
import re
import urllib
import tarfile
import sys



__author__ = "Consciencia"



sys.dont_write_bytecode = True

RESOURCE_DIR = os.path.sep.join([".","resources"])
TARGET_EMACS_SRC_DIR = os.path.sep.join([".","emacs-src"])
RESOURCE_CEDET = RESOURCE_DIR + os.path.sep + "cedet.zip"
TARGET_CEDET_DIR = os.path.sep.join(["lisp", "cedet"])
TARGET_CEDET_BUILD_FILE = os.path.realpath(os.path.sep.join([TARGET_CEDET_DIR,
                                                            "cedet",
                                                            "cedet-build.el"]))



def execute(command, wd=None):
    oldwd = os.getcwd()
    if wd:
        os.chdir(wd)
    os.system(command)
    if wd:
        os.chdir(oldwd)

def getEmacsVersion():
    raw = os.popen("emacs --version").read()
    full = re.search(".*?(\d+\.\d+\.\d+).*",raw).group(1)
    return ".".join(full.split(".")[:2])

def getEmacsSrcURL():
    return "http://mirror.kumi.systems/gnu/emacs/emacs-%s.tar.gz" % getEmacsVersion()

def donwloadEmacsSrc():
    srcArch = TARGET_EMACS_SRC_DIR + os.path.sep + getEmacsVersion() + ".tar.gz"
    targetPath = ".".join(srcArch.split(".")[:-2]) + "_"
    urllib.urlretrieve(getEmacsSrcURL(),
                       srcArch)
    tarfile.open(srcArch).extractall(targetPath)
    shutil.move(targetPath + os.path.sep + "emacs-" + getEmacsVersion(),
                targetPath[:-1])
    os.remove(srcArch)
    shutil.rmtree(targetPath)

def errorRemoveReadonly(func, path, exc):
    excvalue = exc[1]
    if func in (os.rmdir, os.remove) and excvalue.errno == errno.EACCES:
        os.chmod(path, stat.S_IRWXU | stat.S_IRWXG | stat.S_IRWXO)  
        func(path)
    else:
        raise Exception("Failed to delete %s" % path)

def unzip(path, to):
    zip_ref = zipfile.ZipFile(path, 'r')
    zip_ref.extractall(to)
    zip_ref.close()
    
def main():
    if os.path.exists(TARGET_EMACS_SRC_DIR):
        shutil.rmtree(TARGET_EMACS_SRC_DIR, onerror = errorRemoveReadonly)
        os.mkdir(TARGET_EMACS_SRC_DIR)
    else:
        os.mkdir(TARGET_EMACS_SRC_DIR)
    if os.path.exists(TARGET_CEDET_DIR):
        shutil.rmtree(TARGET_CEDET_DIR, onerror = errorRemoveReadonly)
    print("Emacs version " + getEmacsVersion())
    print("Downloading Emacs sources (used for introspection)")
    donwloadEmacsSrc()
    print("Done.")
    print("Buildind CEDET")
    unzip(RESOURCE_CEDET, TARGET_CEDET_DIR)
    execute("emacs -Q -l " + TARGET_CEDET_BUILD_FILE + " -f cedet-build",
            os.path.sep.join(TARGET_CEDET_BUILD_FILE.split(os.path.sep)[:-1]))
    print("Done.")
    print("Installing packages")
    execute("emacs --batch --load init.el")
    print("Done.")
    print("\nIf installation script got here, all should be okay.")
    print("During 'Installing packages' phase, some errors/warnings may appear.")
    print("You may ignore it (If emacs works as expected).")
    print("For example ECB is known to throw some errors during isntallation...")

if __name__ == "__main__":
    main()
