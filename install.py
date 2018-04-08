
#!/usr/bin/env python
# # -*- coding: utf-8 -*-
import os
import shutil
import zipfile
import re
import urllib
import tarfile
import sys
import argparse

__author__ = "Consciencia"

sys.dont_write_bytecode = True

def unzip(path, to):
    zip_ref = zipfile.ZipFile(path, 'r')
    zip_ref.extractall(to)
    zip_ref.close()

def execute(command, wd = None):
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
    version = getEmacsVersion()
    return "http://mirror.kumi.systems/gnu/emacs/emacs-%s.tar.gz" % version

def getEmacsRootConfDir():
    if os.name == "nt":
        raise Exception("Unsupported platform Winsows")
    elif os.name == "posix":
        return os.path.expanduser("~") + os.path.sep + ".emacs.d"
    else:
        raise Exception("Unknown platform %s" % os.name)

def getEmacsConfSourceDir():
    return getEmacsRootConfDir() + os.path.sep + "emacs-src"

def hasSudo():
    return os.name == "posix"

def donwloadEmacsSrc():
    srcArch = getEmacsConfSourceDir() + os.path.sep + getEmacsVersion() + ".tar.gz"
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

def removeTree(path):
    if os.path.exists(path):
        shutil.rmtree(path, onerror = errorRemoveReadonly)

def packageFound(name):
    from distutils.spawn import find_executable
    return find_executable(name) is not None

def actionDownloadEmacsSource():
    print("Downloading Emacs sources (used for introspection):")
    removeTree(getEmacsConfSourceDir())
    os.mkdir(getEmacsConfSourceDir())
    donwloadEmacsSrc()
    print("Done.")

def actionInstallEmacsSources():
    print("Installing EMACS packages:")
    execute("emacs --batch --load init.el")
    print("Done.")

def actionInstallPackages():
    print("Installing packages:")
    packages = [("pylint", "pip install pylint"),
                ("eslint", "npm install -g eslint"),
                ("stylelint", "npm install -g stylelint"),
                ("jsonlint", "npm install -g jsonlint"),
                ("jedi", "pip install jedi"),
                ("flake8", "pip install flake8"),
                ("autopep8", "pip install autopep8"),
                ("yapf", "pip install yapf")]
    for packageName, packageInstall in packages:
        print("\t" + packageName + " - " + packageInstall)
    for packageName, packageInstall in packages:
        print("Installing %s" % packageName)
        if not packageFound(packageName):
            if hasSudo():
                execute("sudo " + packageInstall)
            else:
                execute(packageInstall)
            print("Installed.")
        else:
            print("Found.")
    print("Done.")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--emacsSource",
                        action = "store_true",
                        required = False,
                        help = "Only download EMACS source.")
    parser.add_argument("--emacsPackages",
                        action = "store_true",
                        required = False,
                        help = "Only download EMACS packages.")
    parser.add_argument("--packages",
                        action = "store_true",
                        required = False,
                        help = "Only download system packages.")
    args = parser.parse_args()

    emacsSource = args.emacsSource
    emacsPackages = args.emacsPackages
    packages = args.packages

    print("Hello.")
    print("Emacs version is " + getEmacsVersion() + ".")
    if not os.path.isdir(getEmacsRootConfDir() + os.path.sep + "semanticdb"):
        os.mkdir(getEmacsRootConfDir() + os.path.sep + "semanticdb")
    if emacsSource:
        actionDownloadEmacsSource()
    if emacsPackages:
        actionInstallEmacsSources()
    if packages:
        actionInstallPackages()
    print("EMACS is ready to be used!")
    print("Good flight.")

if __name__ == "__main__":
    main()
