#!/usr/bin/env python
# # -*- coding: utf-8 -*-
import os
import shutil
import zipfile
import re
import urllib.request
import tarfile
import argparse
import subprocess
import sys
import platform
import stat
import errno


__author__ = "Consciencia"


sys.dont_write_bytecode = True


def assertCommands(commands):
    for command in commands:
        try:
            subprocess.call([command, "--help"],
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        except:
            print("Not found command '" + command + "'.")
            if command == "emacs":
                raise Exception("In case of EMACS, you must add its binary into" +
                                " PATH yourself to be visible in command line")


def unzip(path, to):
    zip_ref = zipfile.ZipFile(path, 'r')
    zip_ref.extractall(to)
    zip_ref.close()


def execute(command, wd=None):
    oldwd = os.getcwd()
    if wd:
        os.chdir(wd)
    os.system(command)
    if wd:
        os.chdir(oldwd)


def getInstallCommand():
    if platform.dist()[0] == "fedora":
        return "sudo dnf install -y %s"
    return None


def getEmacsVersion():
    raw = os.popen("emacs --version").read()
    full = re.search(".*?(\\d+\\.\\d+(?:\\.\\d+)?).*", raw).group(1)
    return ".".join(full.split(".")[:2])


def getEmacsSrcURL():
    version = getEmacsVersion()
    return "http://mirror.kumi.systems/gnu/emacs/emacs-%s.tar.gz" % version


def getEmacsRootConfDir():
    command = "emacs" +\
              " --no-init-file" +\
              " --batch" +\
              " --eval" +\
              " \"(print (expand-file-name user-emacs-directory))\""
    return os.popen(command).read()[2:-2]


def getEmacsConfSourceDir():
    return getEmacsRootConfDir() + "emacs-src"


def hasSudo():
    return os.name == "posix"


def donwloadEmacsSrc():
    srcArch = getEmacsConfSourceDir() + os.path.sep +\
              getEmacsVersion() + ".tar.gz"
    targetPath = ".".join(srcArch.split(".")[:-2]) + "_"
    print("GET " + getEmacsSrcURL() + ".")
    urllib.request.urlretrieve(getEmacsSrcURL(),
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
        shutil.rmtree(path, onerror=errorRemoveReadonly)


def packageFound(name):
    from distutils.spawn import find_executable
    return find_executable(name) is not None


def actionDownloadEmacsSource():
    print("Downloading Emacs sources (used for introspection):")
    removeTree(getEmacsConfSourceDir())
    os.mkdir(getEmacsConfSourceDir())
    donwloadEmacsSrc()
    print("Done.")


def actionInstallEmacsPackages():
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
                ("yapf", "pip install yapf"),
                ("tern", "npm install -g tern")]
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


def actionInstallOsPackages():
    print("Installing os packages:")
    command = getInstallCommand()
    packages = ["emacs",
                "global",
                "cscope"]
    if command:
        for package in packages:
            print("Installing %s" % package)
            try:
                execute(command % package)
            except:
                print("Failed to install %s." % package)
            else:
                print("Installed.")
    else:
        print("Skipping, no info for host system %s." % packages)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--info",
                        action="store_true",
                        required=False,
                        help="Display EMACS info and exit.")
    parser.add_argument("--emacsSource",
                        action="store_true",
                        required=False,
                        help="Only download EMACS source.")
    parser.add_argument("--emacsPackages",
                        action="store_true",
                        required=False,
                        help="Only download EMACS packages.")
    parser.add_argument("--osPackages",
                        action="store_true",
                        required=False,
                        help="Only download OS packages.")
    parser.add_argument("--packages",
                        action="store_true",
                        required=False,
                        help="Only download system packages.")
    args = parser.parse_args()

    info = args.info
    emacsSource = args.emacsSource
    emacsPackages = args.emacsPackages
    osPackages = args.osPackages
    packages = args.packages

    allFlags = emacsSource or emacsPackages
    allFlags = allFlags or packages or osPackages
    if not allFlags:
        emacsSource = True
        emacsPackages = True
        packages = True
        osPackages = True

    print("Hello.")
    print("Emacs version is '" + getEmacsVersion() + "'.")
    print("Emacs home is " + getEmacsRootConfDir() + ".")
    if info:
        exit(0)

    if osPackages:
        actionInstallOsPackages()

    assertCommands(["emacs",
                    "git",
                    "npm",
                    "pip"])

    if not os.path.isdir(getEmacsRootConfDir() + "semanticdb"):
        os.mkdir(getEmacsRootConfDir() + "semanticdb")
    print("Setup started.")
    if emacsSource:
        actionDownloadEmacsSource()
    if emacsPackages:
        actionInstallEmacsPackages()
    if packages:
        actionInstallPackages()

    print("Emacs is ready to be used!")
    print("Good flight.")


if __name__ == "__main__":
    main()
