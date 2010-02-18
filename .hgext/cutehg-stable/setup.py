#!/usr/bin/env python

# CuteHg - Qt4 Dialog Extension of Mercurial
# Copyright (C) 2009 Tom Burdick 
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import sys
import os
import shutil
import dircache
import distutils.core
import distutils.util
from distutils.command.build import build
from distutils.command.build_ext import build_ext
from distutils.core import Command
from distutils import log
from subprocess import check_call as call

try:
    from distutils.command.bdist_msi import bdist_msi
    from distutils.command.bdist_wininst import bdist_wininst
except ImportError:
    bdist_msi = None
    bdist_wininst = None

version = '0.0.3'

def needsupdate(src, targ):
    return not os.path.exists(targ) or os.path.getmtime(src) > os.path.getmtime(targ)

def copy_file(src, targ):
    if not needsupdate(src, targ):
        return
    print("copying %s -> %s" % (src, targ))
    shutil.copyfile(src, targ)

def copy_tree(src, targ):
    print("copying %s -> %s" % (src, targ))
    shutil.copytree(src, targ)

def wget(src, targ, user=None, passwd=None, realm=None):
    if os.path.exists(targ):
        return
    print("downloading %s -> %s" % (src, targ))
    import urllib2, urlparse

    if user:
        auth = urllib2.HTTPBasicAuthHandler()
        auth.add_password(realm, urlparse.urlparse(src).netloc, user, passwd)
        opener = urllib2.build_opener(auth)
        urllib2.install_opener(opener)
        urlopen = opener.open
    else:
        pass
    urlopen = urllib2.urlopen
    fsrc = urlopen(src)
        
    ftarg = open(targ, 'wb')
    ftarg.write(fsrc.read())
    ftarg.flush()
    ftarg.close()
    fsrc.close()

def buildWinShellExt():
    builders = sys.path + [
            '%SystemDrive%/Program Files (x86)/Microsoft Visual Studio 9.0/VC/vcpackages/',
            '%SystemDrive%/Program Files/Microsoft Visual Studio 9.0/VC/vcpackages/',
            ]

    from os.path import join, exists, expandvars
    builders = map(lambda p: join(expandvars(p), "vcbuild.exe"), builders)
    builders = [p for p in builders if exists(p)]

    if len(builders) == 0:
        raise distutils.errors.DistutilsExecError, "Could not find vcbuild.exe"

    import subprocess
    rccprocess = subprocess.Popen([builders[0], 'win32/shellext/shellext.vcproj'])
    rccprocess.wait()

    copy_file('build/cutehg-shellext-Win32-Release.dll', 'build/cutehg-32.dll')
    copy_file('build/cutehg-shellext-x64-Release.dll', 'build/cutehg-64.dll')

class build_winext(build_ext):
    def run(self):
        buildWinShellExt()
        build_ext.run(self)

class CuteHgBuildUi(Command):
    description = "build Python modules from Qt Designer .ui files"

    user_options = []

    def initialize_options(self):
        pass

    def finalize_options(self):
        pass

    def compile_ui(self, ui_file, py_file):
        if not needsupdate(ui_file, py_file):
            return
        print("compiling %s -> %s" % (ui_file, py_file))
        try:
            from PyQt4 import uic
            fp = open(py_file, 'w')
            uic.compileUi(ui_file, fp)
            fp.close()
        except Exception, e:
            raise distutils.errors.DistutilsExecError, 'Unable to compile user interface %s' % str(e)
            return
    
    def compile_qrc(self, qrc_file, py_file):
        if not needsupdate(qrc_file, py_file):
            return
        print("compiling %s -> %s" % (qrc_file, py_file))
        try:
            import subprocess
            rccprocess = subprocess.Popen(['pyrcc4', qrc_file, '-o', py_file])
            rccprocess.wait()
        except Exception, e:
            raise distutils.errors.DistutilsExecError, 'Unable to compile resouce file %s' % str(e)
            return

    def run(self):
        ui_info = (
                (self.compile_ui, 'ui', dircache.listdir('cutehg/ui'), 'cutehg/ui/%s.ui', 'cutehg/dialogs/ui_%s.py'),
                (self.compile_ui, 'ui', dircache.listdir('cutehg/views/layouts'), 'cutehg/views/layouts/%s.ui', 'cutehg/views/ui_%s.py'),
            )

        for action, ext, ui_files, src, targ in ui_info:
            for filename in ui_files:
                parts = filename.rsplit('.', 1)
                if len(parts) < 2 or parts[1] != ext:
                    continue
                name = parts[0]
                action((src % name), (targ % name))

        self.compile_qrc( 'cutehg/icons/icons.qrc', 'cutehg/icons_rc.py' )
        copy_file( 'cutehg/icons_rc.py', 'cutehg/dialogs/icons_rc.py' )
        copy_file( 'cutehg/icons_rc.py', 'cutehg/views/icons_rc.py' )

def hg(command, *args, **kwargs):
    """
    gopts: Global options, set before the command
    sopts: Options without arguments for command
    opts:  Options with arguments for command
    
    passed to subprocess:
    cwd:   Working directory to execute hg
    """
    def option(name, value=None):
        if len(name) == 1:
            return '-' + name
        else:
            return '--' + name
    gopts = []
    for n, v in kwargs.get('gopts', {}).iteritems():
        gopts.extend([option(n), v])
    opts  = [option(n) for n in kwargs.get('sopts', [])]
    for n, v in kwargs.get('opts', {}).iteritems():
        opts.extend([option(n), v])
    cmdline = ['hg'] + gopts + [command] + opts + list(args)
    cwd = kwargs.get('cwd', None)
    if cwd:
        print('executing (in %s): %s' % (cwd, ' '.join(cmdline)))
        call(cmdline, cwd=cwd)
    else:
        print('executing: ' + ' '.join(cmdline))
        call(cmdline)

msvcpdll = 'msvcp90.dll'

def require_update(target, dependencies):
    from os.path import join, exists, getmtime
    if not exists(target):
        return True
    mtime = getmtime(target)
    for dep in dependencies:
        if not exists(dep):
            raise IOError("File %s does not exist, cannot check mtime dependencies for %s" % (dep, target))
        if getmtime(dep) > mtime:
            return True
    return False

class AsciiDoc(object):
    executable = None
    def __init__(self, executable):
        self.executable = executable

    def __call__(self, target, backend, cwd=None):
        cmdline = self.executable + ['-b', backend, target]
        print('asciidoc: ' + ' '.join(cmdline))
        call(cmdline, cwd=cwd)

    @staticmethod
    def find_executable():
        from os.path import join, exists, expandvars
        locations = sys.path + [
                sys.prefix,
                ]
        asciidoc = [join(expandvars(p), 'asciidoc.exe') for p in locations] + \
                   [join(expandvars(p), 'asciidoc.bat') for p in locations] + \
                   [join(expandvars(p), 'asciidoc.py') for p in locations]
        env = os.environ.get('ASCIIDOC', None)
        if env:
            asciidoc = [env] + asciidoc
        asciidoc = [p for p in asciidoc if exists(p)]
        if not asciidoc:
            raise distutils.errors.DistutilsExecError('Could not find asciidoc executable')
        asciidoc = asciidoc[0]
        if asciidoc.endswith('.py'):
            return [sys.executable, asciidoc]
        else:
            return [asciidoc]

# TODO: The build directory should be configurable
# TODO: See if this command can be built differently, like ISS command
class CuteHgPy2Exe(build):
    description = "create a windows executable that can run mercurial and cutehg"

    def run(self):
        try:
            import py2exe
        except ImportError:
            raise distutils.errors.DistutilsExecError('Py2exe is not installed, cannot build installer')
        # TODO: Check for forest extension
        from os.path import join, exists, expandvars
        from os import getcwd
        from shutil import rmtree

        # Find asciidoc executable
        asciidoc = AsciiDoc(AsciiDoc.find_executable())

        # Run sub_commands to build cutehg
        build.run(self)

        dist_path = join('build', 'hg-cutehg')

        # TODO: Only seed to a given tag/rev
        hg('fseed', 'installer/devel.snap')
        hg('fpull', shortopts=['update'], opts={'snapfile': 'installer/devel.snap'})

        if not exists('build'):
            os.makedirs('build')

        # Prepare a folder where the distribution will be placed
        if exists('build/hg-cutehg'):
            if exists('build/hg-cutehg/.hg/patches'):
                hg('qpop', sopts=['all'], cwd='build/hg-cutehg')
            hg('pull', join(getcwd(), 'hg-main'), cwd='build/hg-cutehg')
            hg('purge', gopts={'config': 'extensions.hgext.purge='}, cwd='build/hg-cutehg')
            hg('update', cwd='build/hg-cutehg')
        else:
            hg('clone', 'hg-main', 'build/hg-cutehg')

        # install cutehg as an extension
        if exists('build/hg-cutehg/hgext/cutehg'):
            rmtree('build/hg-cutehg/hgext/cutehg')
        copy_tree('cutehg', 'build/hg-cutehg/hgext/cutehg')

        # Make sure hg itself is built (C-extension etc.)
        call([sys.executable, 'setup.py', 'build'], cwd='build/hg-cutehg')

        # Copy over DLL needed by PyQt4
        dllpath = [
                '%SystemDrive%/Program Files/Microsoft Visual Studio 9.0/VC/redist/x86/Microsoft.VC90.CRT',
                ] + sys.path

        fullpath = [join(expandvars(p), msvcpdll) for p in dllpath]
        fullpath = [p for p in fullpath if exists(p)]
        if not fullpath:
            raise distutils.errors.DistutilsExecError('Could not find msvcp90.dll')
        copy_file(fullpath[0], 'build/hg-cutehg/' + msvcpdll)

        # Patch hg to include PyQt4 and cutehg
        if exists('build/hg-cutehg/.hg/patches'):
            rmtree('build/hg-cutehg/.hg/patches')
        copy_tree('installer/hg-patches', 'build/hg-cutehg/.hg/patches')
        hg('qpush', sopts=['all'], cwd='build/hg-cutehg')

        # Copy icon used by py2exe
        copy_file('installer/cutehg.ico', 'build/hg-cutehg/cutehg.ico')

        # Build build/hg-cutehg with py2exe
        call([sys.executable, 'setup.py', 'py2exe'], cwd='build/hg-cutehg')

        # Copy over DLL needed for post-installation
        for postdll in ('cutehg-32.dll', 'cutehg-64.dll'):
            if exists('build/' + postdll):
                copy_file('build/' + postdll, 'build/hg-cutehg/dist/' + postdll)
            elif exists(postdll):
                copy_file(postdll, 'build/hg-cutehg/dist/' + postdll)
            else:
                raise distutils.errors.DistutilsExecError('Could not find ' + postdll)

        # Prepare HTML documentation
        doc_path = join(dist_path, 'doc')
        if require_update(join(doc_path, 'hg.1.gendoc.txt'), [join(dist_path, 'mercurial', name) for name in ('commands.py', 'help.py')]):
            call([sys.executable, 'gendoc.py'], stdout=open(join(doc_path, 'hg.1.gendoc.txt'), 'w'), cwd=doc_path)
        for doc in (e for e in os.listdir(doc_path) if e.endswith('.txt')):
            html = join(doc_path, doc[:-4] + '.html')
            if require_update(html, (join(doc_path, doc), join(doc_path, 'hg.1.gendoc.txt'))):
                asciidoc(doc, 'html4', cwd=doc_path)

    sub_commands = [("build", None)]

class CuteHgBdistIntaller(Command):
    user_options = [('dist-dir=', 'd',
                     "directory to put final built installer in"),
                    ('skip-build', None,
                     "skip rebuilding everything (for testing/debugging)"),
                    ('force-download-overlays', None,
                     "force downloading tortoise overlays even if they are present locally"),
                   ]

    boolean_options = ['skip-build', 'force-download']

    def initialize_options(self):
        self.dist_dir = None
        self.skip_build = 0
        self.force_download_overlays = 0

    def finalize_options(self):
        if not self.dist_dir:
            self.dist_dir = "dist"

    def run(self):
        from os.path import join, exists, expandvars
        if not self.skip_build:
            self.run_command('py2exe')

        if not exists('build/hg-cutehg'):
            raise distutils.errors.DistutilsFileError('Build directory %s was not found, cutehg has most likely not been built' % 'build/hg-cutehg')

        # get tortoise overlays
        url = 'http://tortoisesvn.tigris.org/svn/tortoisesvn/TortoiseOverlays/version-1.0.5/bin/TortoiseOverlays-1.0.5.14479-%s.msi'
        for winplat in ['win32', 'x64']:
            target = 'build/hg-cutehg/dist/tortoiseoverlays-%s.msi' % winplat
            if self.force_download_overlays or not exists(target):
                if exists(target):
                    os.remove(target)
                log.info("downloading tortoise overlay for %s to build/hg-cutehg/dist", winplat)
                wget(url % winplat, target, 'guest', '', 'CollabNet Subversion Repository')

        # Make the installer
        if not exists(self.dist_dir):
            os.makedirs(self.dist_dir)

class CuteHgBdistISS(CuteHgBdistIntaller):
    description = "create an executable installer for MS Windows using Inno Setup"

    def run(self):
        from os.path import join, exists, expandvars
        iscc = sys.path + [
                '%ProgramFiles%/Inno Setup 5',
                '%SystemDrive%/Program Files/Inno Setup 5',
                ]
        iscc = [join(expandvars(p), 'iscc.exe') for p in iscc]
        iscc = [p for p in iscc if exists(p)]
        if not iscc:
            raise distutils.errors.DistutilsExecError('Could not find iscc.exe')

        CuteHgBdistIntaller.run(self)

        build_path = join('build', 'hg-cutehg')
        dist_path = join(build_path, 'dist')

        # Donwload some required dll/exe files
        # TODO: This should be performed as callbacks when the file is not found locally
        # TODO: Find a good url for downloading msvcr71.dll from the web, maybe put it on bitbucket? 
        installer_file = 'mfc71.dll'
        if not exists(installer_file):
            wget('http://starship.python.net/crew/mhammond/downloads/mfc71.dll', installer_file)

        installer_file = 'add_path.exe'
        if not exists(installer_file):
            if not exists('add_path.zip'):
                wget('http://www.barisione.org/files/add_path.zip', 'add_path.zip')
            from zipfile import ZipFile
            f = ZipFile('add_path.zip')
            f.extract(installer_file)

        # Copy some files required by the installer
        log.info('copying dlls/exes required by installer')
        for installer_file in ('mfc71.dll', 'msvcr71.dll', 'add_path.exe'):
            found = False
            for path in ('', '%SystemRoot%/system32/', ):
                if exists(expandvars(join(path, installer_file))):
                    copy_file(expandvars(join(path, installer_file)), join(dist_path, installer_file))
                    found = True
                    break
            if not found:
                raise distutils.errors.DistutilsExecError('Could not find ' + installer_file)

        # Creating the .iss file
        hg_iss_file = join(build_path, 'contrib', 'win32', 'mercurial.iss')
        out = open(hg_iss_file, 'rb').read()
        import re
        replacements = {'AppCopyright': 'CuteHg Copyright (C) 2009 Tom Burdick, Mercurial Copyright 2005-2009 Matt Mackall and others',
                        'AppName': 'CuteHg',
                        'AppVerName': 'CuteHg-%s' % version,
                        'AppPublisher': 'Tom Burdick and others',
                        'AppPublisherURL': 'http://bitbucket.org/bfrog/cutehg-stable/wiki/Home',
                        'AppSupportURL': 'http://bitbucket.org/bfrog/cutehg-stable/wiki/Home',
                        'AppUpdatesURL': 'http://bitbucket.org/bfrog/cutehg-stable/wiki/Home',
                        'AppID': '{{52E114B1-5806-4E75-9D7C-24369BDAACAF}',
                        'AppContact': 'cutehg@googlegroups.com',
                        'OutputBaseFilename': 'CuteHg-%s' % version,
                        'DefaultDirName': '{pf}\\\\CuteHg',
                        'SourceDir': '..',
                        'VersionInfoDescription': 'Mercurial distributed SCM and CuteHg GUI',
                        'VersionInfoCopyright': 'CuteHg Copyright (C) 2009 Tom Burdick, Mercurial Copyright 2005-2009 Matt Mackall and others',
                        'VersionInfoCompany': 'Tom Burdick and others',
                        'DefaultGroupName': 'CuteHg',
                        'PrivilegesRequired': 'poweruser',
                        }
        for k, v in replacements.iteritems():
            out = re.compile('^%s=.*' % k, re.I | re.M).sub('%s=%s' % (k, v), out)
        out = re.compile('^Name: \\{group\\}\\\\Uninstall Mercurial;.*$', re.I | re.M).sub('', out)
        iss = join('dist', 'mercurial-orig.iss')
        open(join(build_path, iss), "wb").write(out)

        # Creating the .iss file
        iss_file = join('installer', 'mercurial.iss')
        iss = join('dist', 'mercurial.iss')
        log.info("preparing installer: %s" % iss)
        from string import Template
        t = Template(open(iss_file, "rb").read())
        out = t.safe_substitute(version=version, path='..')
        open(join(build_path, iss), "wb").write(out)

        # Build installer with Inno Setup
        log.info("creating installer using Inno Setup")
        cmdline = [iscc[0], iss]
        log.info(' '.join(cmdline))
        call(cmdline, cwd=build_path)

        # Copy it to the root
        log.info("copying installer to current directory")
        copy_file(join(build_path, 'Output', 'CuteHg-%s.exe' % version), 'CuteHg-%s.exe' % version)

def MakeWinBdist(bdist_base):
    class bdist_win(bdist_base):
        def initialize_options(self):
            bdist_base.initialize_options(self)
            self.distribution.data_files = []

            # force UAC on vista
            self.user_access_control = 'force'
            
            # include dlls
            self.distribution.data_files.append(
                    ('', ['build/cutehg-32.dll', 'build/cutehg-64.dll'])
                    )

            # get tortoise overlays
            url = 'http://tortoisesvn.tigris.org/svn/tortoisesvn/TortoiseOverlays/version-1.0.5/bin/TortoiseOverlays-1.0.5.14479-%s.msi'
            for type in ['win32', 'x64']:
                target = 'build/tortoiseoverlays-%s.msi' % type
                wget(url % type, target, 'guest', '', 'CollabNet Subversion Repository')
                self.distribution.data_files.append(('', [target]))

            # include registration script
            self.distribution.scripts = ['win32/postinstall.py']
            self.install_script = 'postinstall.py'
    return bdist_win

class CuteHgBuild(build):
    def is_win_platform(self):
        return hasattr(self, "plat_name") and (self.plat_name[:3] == 'win')

    sub_commands = [('build_ui', None)] + build.sub_commands + [('build_winext', is_win_platform)]

cmds = {
        'build' : CuteHgBuild,
        'build_ui' : CuteHgBuildUi,
        'build_winext' : build_winext,
        'py2exe' : CuteHgPy2Exe,
       }

if bdist_wininst:
    CuteHgWinInst = MakeWinBdist(bdist_wininst)
    CuteHgWinMsi = MakeWinBdist(bdist_msi)
    cmds['bdist_wininst'] = CuteHgWinInst
    cmds['bdist_msi'] = CuteHgWinMsi
    cmds['bdist_iss'] = CuteHgBdistISS


distutils.core.setup(
    name='cutehg',
    version=version,
    description='Qt4 Dialog extension to Mercurial',
    author="Tom Burdick",
    author_email='thomas.burdick@gmail.com',
    license='GNU GPL2',
    cmdclass = cmds,
    platforms='All',
    packages=['cutehg', 'cutehg.dialogs', 'cutehg.views', 'cutehg.views.models'],
    url='http://www.bitbucket.org/bfrog/cutehg',
    )
