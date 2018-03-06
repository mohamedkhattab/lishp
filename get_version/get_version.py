import os, subprocess, runpy

# Try to get and update the version.

os.chdir(os.path.split(os.path.abspath(__file__))[0])
VERSIONFILE = os.path.join("hy", "version.py")

try:
    __version__ = (subprocess.check_output
                   (["git", "describe", "--tags", "--dirty"])
                   .decode('ASCII').strip()
                   .replace('-', '+', 1).replace('-', '.'))
    with open(VERSIONFILE, "wt") as o:
        o.write("__version__ = {!r}\n".format(__version__))

except (subprocess.CalledProcessError, OSError):
    if os.path.exists(VERSIONFILE):
        __version__ = runpy.run_path(VERSIONFILE)['__version__']
    else:
        __version__ = "unknown"
